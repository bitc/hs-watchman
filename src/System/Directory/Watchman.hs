{-# LANGUAGE OverloadedStrings #-}

module System.Directory.Watchman
    ( version
    , shutdownServer
    , WatchmanVersion(..)
    , WatchResponse(..)
    , WatchmanSocket
    , WatchmanSubscription
    , watch
    , query
    , withConnect
    , subscribe
    , unsubscribe
    , stateEnter
    , stateLeave
    , readNotification
    , watchList
    ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (bracket, bracketOnError)
import Control.Exception (throwIO, try)
import Control.Monad (unless, forever)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified Network.Socket as Net

import System.Directory.Watchman.BSER
import System.Directory.Watchman.BSER.Parser
import System.Directory.Watchman.BSER.Protocol
import System.Directory.Watchman.Expression (Expression)
import System.Directory.Watchman.Fields
import System.Directory.Watchman.Query
import System.Directory.Watchman.State
import System.Directory.Watchman.Subscribe
import System.Directory.Watchman.Types
import System.Directory.Watchman.WFilePath
import System.Directory.Watchman.WatchmanException

newtype WatchmanWarning = WatchmanWarning String
    deriving (Show, Eq, Ord)

data WatchmanCommand a b = WatchmanCommand (a -> BSERValue) (BSERValue -> Parser b)

newtype WatchmanVersion = WatchmanVersion String
    deriving (Show, Eq, Ord)

instance FromBSER WatchmanVersion where
    parseBSER (BSERObject o) = do
        v <- o .: "version"
        pure (WatchmanVersion (BC.unpack v))
    parseBSER _ = fail "Not an Object"

versionCmd :: WatchmanCommand () WatchmanVersion
versionCmd =
    WatchmanCommand
        (const $ BSERArray (Seq.singleton (BSERString (BC.pack "version"))))
        parseBSER

version :: WatchmanSockFile -> IO WatchmanVersion
version sockFile = runCommand sockFile versionCmd ()

-- | Checks to see if the response from the watchman server is an error response
readError :: BSERValue -> Maybe String
readError (BSERObject o) =
    case M.lookup "error" o of
        Just (BSERString errStr) -> Just (BC.unpack errStr)
        _ -> Nothing
readError _ = Nothing

runCommand :: WatchmanSockFile -> WatchmanCommand a b -> a -> IO b
runCommand sockFile (WatchmanCommand buildInput parseOutput) args = do
    bracket
        (connectToWatchman sockFile)
        disconnectWatchman
        $ \(WatchmanConnection sock) -> do
            let m = buildInput args
            sendBSERMessage sock m
            rsp <- readBSERMessage sock
            case readError rsp of
                Just err -> throwIO $ WatchmanException_ErrorResponse err
                Nothing -> case parse parseOutput rsp of
                    Error err -> fail err
                    Success result -> pure result

connectToWatchman :: WatchmanSockFile -> IO WatchmanConnection
connectToWatchman sockFile = do
    tryResult <- try $ bracketOnError
        (Net.socket Net.AF_UNIX Net.Stream 0)
        Net.close
        (\sock -> do
            Net.connect sock (Net.SockAddrUnix sockFile)
            pure (WatchmanConnection sock))
    case tryResult of
        Left ex -> throwIO $ WatchmanException_SockError ex
        Right c -> pure c

disconnectWatchman :: WatchmanConnection -> IO ()
disconnectWatchman (WatchmanConnection sock) = Net.close sock

shutdownServerCmd :: WatchmanCommand () ShutdownServer
shutdownServerCmd = WatchmanCommand
    (const $ BSERArray (Seq.singleton (BSERString (BC.pack "shutdown-server"))))
    parseBSER

newtype ShutdownServer = ShutdownServer Bool
    deriving (Show, Eq, Ord)

instance FromBSER ShutdownServer where
    parseBSER (BSERObject o) = do
        v <- o .: "shutdown-server"
        pure (ShutdownServer v)
    parseBSER _ = fail "Not an Object"


shutdownServer :: WatchmanSockFile -> IO ShutdownServer
shutdownServer sockFile = runCommand sockFile shutdownServerCmd ()

data WatchResponse = WatchResponse
    { _WatchResponse_Watch :: WFilePath
    , _WatchResponse_Watcher :: String
    }
    deriving (Show, Eq)

instance FromBSER WatchResponse where
    parseBSER (BSERObject o) = do
        watch_ <- o .: "watch"
        watcher <- o .: "watcher"
        pure WatchResponse
            { _WatchResponse_Watch = WFilePath watch_
            , _WatchResponse_Watcher = BC.unpack watcher
            }
    parseBSER _ = fail "Not an Object"

watchCmd :: WatchmanCommand WFilePath WatchResponse
watchCmd = WatchmanCommand
    (\filepath -> BSERArray (Seq.fromList [BSERString (BC.pack "watch"), BSERString (toByteString filepath)]))
    parseBSER

watch :: WatchmanSockFile -> WFilePath -> IO WatchResponse
watch sockFile filepath = runCommand sockFile watchCmd filepath

parseRoots :: BSERValue -> Parser [WFilePath]
parseRoots (BSERObject o) = do
    roots <- o .: "roots"
    mapM parseBSER roots
parseRoots _ = fail "Not an Object"

watchListCmd :: WatchmanCommand () [WFilePath]
watchListCmd = WatchmanCommand
    (const $ BSERArray (Seq.singleton (BSERString (BC.pack "watch-list"))))
    parseRoots

watchList :: WatchmanSockFile -> IO [WFilePath]
watchList sockFile = runCommand sockFile watchListCmd ()

queryCmd :: [FileFieldLabel] -> WatchmanCommand (WFilePath, [Generators -> Generators], Expression, [QueryParams -> QueryParams]) QueryResult
queryCmd fileFieldLabels = WatchmanCommand
    (\(p, g, e, q) -> renderQuery p g e q fileFieldLabels)
    (parseQueryResult fileFieldLabels)

query
    :: WatchmanSockFile
    -> WFilePath
    -> [Generators -> Generators]
    -> Expression
    -> [QueryParams -> QueryParams]
    -> [FileFieldLabel] -- ^ Must not be empty. Must not have duplicates
    -> IO QueryResult
query sockFile filepath generators expr queryParams fileFields = runCommand sockFile (queryCmd fileFields) (filepath, generators, expr, queryParams)

data WatchmanSocket = WatchmanSocket !Net.Socket !ThreadId !(MVar (MVar BSERValue)) !(MVar (Map SubscriptionName ([FileFieldLabel], Chan SubscriptionNotification)))

connect :: WatchmanSockFile -> IO WatchmanSocket
connect sockFile = do
    cmdRspVar <- newEmptyMVar
    subscriptions <- newMVar M.empty
    bracketOnError
        (connectToWatchman sockFile)
        disconnectWatchman
        $ \(WatchmanConnection sock) -> bracketOnError
            (forkIO $ readThread sock cmdRspVar subscriptions)
            killThread
            $ \threadId -> do
                pure $ WatchmanSocket sock threadId cmdRspVar subscriptions
    where
    readThread sock cmdRspVar subscriptionsVar = forever $ do
        -- TODO Handle any errors
        rsp <- readBSERMessage sock
        case subscriptionNotification rsp of
            Just subscription -> do
                withMVar subscriptionsVar $ \subscriptions -> do
                    case M.lookup subscription subscriptions of
                        Nothing ->
                            -- We got a notification for a subscription that does not exist. It must have been just un-subscribed (while this notification was already on route).
                            -- We can just ignore the message
                            pure ()
                        Just (fileFieldLabels, chan) -> do
                            case parse (parseSubscriptionNotification fileFieldLabels) rsp of
                                Error _err ->
                                    -- TODO ...
                                    error "TODO 62462"
                                Success result -> do
                                    writeChan chan result
            Nothing -> do
                mbVar <- tryTakeMVar cmdRspVar
                case mbVar of
                    Nothing -> do
                        -- TODO We received an unexpected message: It wasn't a subscription notification, and we don't have an in-flight command that is expecting a response.
                        -- This should probably be dealt with in the same way that we deal with an error from 'readBSERMessage' above
                        error "TODO 2392362"
                    Just var -> do
                        -- When a command gives us an MVar, it is required to be an empty MVar
                        sanityCheck <- tryPutMVar var rsp
                        unless sanityCheck $
                            fail "The Impossible happened!"
    -- | Checks to see if the response from the watchman server is an error response
    subscriptionNotification :: BSERValue -> Maybe SubscriptionName
    subscriptionNotification (BSERObject o) =
        case M.lookup "subscription" o of
            Just (BSERString s) -> Just (SubscriptionName s)
            Just _ -> Nothing
            Nothing -> Nothing
    subscriptionNotification _ = Nothing

disconnect :: WatchmanSocket -> IO ()
disconnect (WatchmanSocket sock readThread _ _) = do
    -- TODO Maybe we also need to do something about the MVars?
    killThread readThread
    disconnectWatchman (WatchmanConnection sock)

withConnect :: WatchmanSockFile -> (WatchmanSocket -> IO a) -> IO a
withConnect sockFile = bracket (connect sockFile) disconnect

data WatchmanSubscription = WatchmanSubscription !(IO ()) !(IO SubscriptionNotification)

subscribe :: WatchmanSocket -> WFilePath -> SubscriptionName -> Expression -> [SubscribeParams -> SubscribeParams] -> [FileFieldLabel] -> IO WatchmanSubscription
subscribe (WatchmanSocket sock _ cmdRspVar subscriptionsVar) filepath subscriptionName expr subscribeParams fileFields = do
    rspVar <- newEmptyMVar

    notificationsChan <- newChan

    -- TODO need 'bracketOnError' that will erase the subscription from the map
    modifyMVar_ subscriptionsVar $ \subscriptions -> do
        case M.lookup subscriptionName subscriptions of
            Nothing -> do
                let subscriptions' = M.insert subscriptionName (fileFields, notificationsChan) subscriptions
                pure subscriptions'
            Just _ -> throwIO $ WatchmanException_SubscriptionExists subscriptionName

    -- Give the read thread a place to put the response into. This also acts as a lock(mutex) that will block any concurrent commands until the response is received:
    putMVar cmdRspVar rspVar

    let msg = renderSubscribe filepath subscriptionName expr subscribeParams fileFields
    -- TODO What happens if an async exception happens here? A partial send will leave the socket in a broken state. Might want to use 'uninterruptibleMask_' here.
    -- Then again, I think that 'sendBSERMessage' is an atomic operation.
    sendBSERMessage sock msg

    -- Wait for the read thread to put the response into the place we gave it:
    _rsp <- readMVar rspVar
    -- TODO Check if rsp is 'readError'
    let unsubscribe_ = do
            -- TODO Send message to sock
            -- TODO Remove subscription from Map
            pure (error "TODO 92834252642")
    let next = readChan notificationsChan
    pure $ WatchmanSubscription unsubscribe_ next

unsubscribe :: WatchmanSubscription -> IO ()
unsubscribe (WatchmanSubscription unsubscribe_ _) = unsubscribe_

readNotification :: WatchmanSubscription -> IO SubscriptionNotification
readNotification (WatchmanSubscription _ next) = next

stateEnter :: WatchmanSocket -> WFilePath -> StateName -> [StateParams -> StateParams] -> IO ()
stateEnter (WatchmanSocket sock _ cmdRspVar _) filepath stateName stateParams = do
    rspVar <- newEmptyMVar

    -- Give the read thread a place to put the response into. This also acts as a lock(mutex) that will block any concurrent commands until the response is received:
    putMVar cmdRspVar rspVar

    let msg = renderStateEnter filepath stateName stateParams
    -- TODO What happens if an async exception happens here? A partial send will leave the socket in a broken state. Might want to use 'uninterruptibleMask_' here.
    -- Then again, I think that 'sendBSERMessage' is an atomic operation.
    sendBSERMessage sock msg

    -- Wait for the read thread to put the response into the place we gave it:
    rsp <- readMVar rspVar
    -- TODO Check if rsp is 'readError'
    case readError rsp of
        Just err -> throwIO $ WatchmanException_ErrorResponse err
        Nothing ->
            -- "state-enter" response has fields: "clock", "root", "state-enter", "version".
            --
            -- We aren't
            pure ()

stateLeave :: WatchmanSocket -> WFilePath -> StateName -> [StateParams -> StateParams] -> IO ()
stateLeave (WatchmanSocket sock _ cmdRspVar _) filepath stateName stateParams = do
    rspVar <- newEmptyMVar

    -- Give the read thread a place to put the response into. This also acts as a lock(mutex) that will block any concurrent commands until the response is received:
    putMVar cmdRspVar rspVar

    let msg = renderStateLeave filepath stateName stateParams
    -- TODO What happens if an async exception happens here? A partial send will leave the socket in a broken state. Might want to use 'uninterruptibleMask_' here.
    -- Then again, I think that 'sendBSERMessage' is an atomic operation.
    sendBSERMessage sock msg

    -- Wait for the read thread to put the response into the place we gave it:
    rsp <- readMVar rspVar
    -- TODO Check if rsp is 'readError'
    case readError rsp of
        Just err -> throwIO $ WatchmanException_ErrorResponse err
        Nothing ->
            -- "state-enter" response has fields: "clock", "root", "state-enter", "version".
            --
            -- We aren't
            pure ()
