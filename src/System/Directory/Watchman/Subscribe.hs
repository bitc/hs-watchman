{-# LANGUAGE OverloadedStrings #-}

module System.Directory.Watchman.Subscribe
    ( SubscriptionName(..)
    , SubscribeParams
    , SubscriptionNotification(..)
    , SubscriptionFiles(..)
    , SubscriptionStateEnter(..)
    , SubscriptionStateLeave(..)
    , renderSubscribe
    , parseSubscriptionNotification

    , since
    , deferVcs
    , defer
    , System.Directory.Watchman.Subscribe.drop
    ) where

import Data.Foldable (foldl')
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.ByteString.Char8 as BC8

import System.Directory.Watchman.WFilePath

import System.Directory.Watchman.Fields
import System.Directory.Watchman.Expression (Expression, renderExpression)
import System.Directory.Watchman.BSER
import System.Directory.Watchman.BSER.Parser
import System.Directory.Watchman.Clockspec
import System.Directory.Watchman.State

data SubscribeParams = SubscribeParams
    { _SubscribeParams_Since :: !(Maybe Clockspec)
    , _SubscribeParams_DeferVcs :: !Bool
    , _SubscribeParams_Defer :: ![StateName]
    , _SubscribeParams_Drop :: ![StateName]
    }
    deriving (Show, Eq)

defaultSubscribeParams :: SubscribeParams
defaultSubscribeParams = SubscribeParams
    { _SubscribeParams_Since = Nothing
    , _SubscribeParams_DeferVcs = True
    , _SubscribeParams_Defer = []
    , _SubscribeParams_Drop = []
    }

newtype SubscriptionName = SubscriptionName ByteString
    deriving (Show, Eq, Ord)

-- | The subscribe command object allows the client to specify a since parameter; if present in the command,
-- the initial set of subscription results will only include files that changed since the specified clockspec,
-- equivalent to using the @query@ command with the @since@ generator.
--
-- <https://facebook.github.io/watchman/docs/cmd/subscribe.html>
since :: Clockspec -> (SubscribeParams -> SubscribeParams)
since s x = x { _SubscribeParams_Since = Just s }

-- | Starting in watchman version 3.2, after the notification stream is complete, if the root appears to
-- be a version control directory, subscription notifications will be held until an outstanding version
-- control operation is complete (at the time of writing, this is based on the presence of either
-- @.hg/wlock@ or @.git/index.lock@). This behavior matches triggers and helps to avoid performing transient
-- work in response to files changing, for example, during a rebase operation.
--
-- In some circumstances it is desirable for a client to observe the creation of the control files at the start
-- of a version control operation. You may specify that you want this behavior by using 'deferVcs False'
deferVcs :: Bool -> (SubscribeParams -> SubscribeParams)
deferVcs s x = x { _SubscribeParams_DeferVcs = s }

defer :: [StateName] -> (SubscribeParams -> SubscribeParams)
defer [] _ = error "defer: List of StateNames must not be empty"
defer s x = x { _SubscribeParams_Defer = s }

drop :: [StateName] -> (SubscribeParams -> SubscribeParams)
drop [] _ = error "drop: List of StateNames must not be empty"
drop s x = x { _SubscribeParams_Drop = s }

renderSubscribe :: WFilePath -> SubscriptionName -> Expression -> [SubscribeParams -> SubscribeParams] -> [FileFieldLabel] -> BSERValue
renderSubscribe rootPath (SubscriptionName subscriptionName) expr params fileFieldLabels =
    BSERArray $ Seq.fromList
        [ BSERString "subscribe"
        , BSERString (toByteString rootPath)
        , BSERString subscriptionName
        , BSERObject $ M.unions
            [ renderFieldLabels fileFieldLabels
            , M.singleton "expression" (renderExpression expr)
            , renderSubscribeParams params'
            ]
        ]
    where
    params' = foldl' (\x f -> f x) defaultSubscribeParams params

renderSubscribeParams :: SubscribeParams -> Map ByteString BSERValue
renderSubscribeParams params = M.unions
    [ case _SubscribeParams_Since params of
        Nothing -> M.empty
        Just c -> M.singleton "since" (renderClockspec c)
    , case _SubscribeParams_DeferVcs params of
        True -> M.empty -- When not specified, default behaviour of watchman is True
        False -> M.singleton "defer_vcs" (BSERBool False)
    , case _SubscribeParams_Defer params of
        [] -> M.empty
        xs -> M.singleton "defer" (BSERArray (Seq.fromList (map (\(StateName s) -> BSERString s) xs)))
    , case _SubscribeParams_Drop params of
        [] -> M.empty
        xs -> M.singleton "drop" (BSERArray (Seq.fromList (map (\(StateName s) -> BSERString s) xs)))
    ]

data SubscriptionFiles = SubscriptionFiles
    { _SubscriptionFiles_Clock :: !ClockId
    , _SubscriptionFiles_Root :: !WFilePath
    , _SubscriptionFiles_Subscription :: !SubscriptionName
    , _SubscriptionFiles_Files :: !(Seq [FileField])
    , _SubscriptionFiles_IsFreshInstance :: !Bool
    }
    deriving (Show, Eq, Ord)

data SubscriptionStateEnter = SubscriptionStateEnter
    { _SubscriptionStateEnter_Clock :: !ClockId
    , _SubscriptionStateEnter_Root :: !WFilePath
    , _SubscriptionStateEnter_Subscription :: !SubscriptionName
    , _SubscriptionStateEnter_State :: !StateName
    , _SubscriptionStateEnter_Metadata :: !(Maybe BSERValue)
    }
    deriving (Show, Eq, Ord)

data SubscriptionStateLeave = SubscriptionStateLeave
    { _SubscriptionStateLeave_Clock :: !ClockId
    , _SubscriptionStateLeave_Root :: !WFilePath
    , _SubscriptionStateLeave_Subscription :: !SubscriptionName
    , _SubscriptionStateLeave_State :: !StateName
    , _SubscriptionStateLeave_Metadata :: !(Maybe BSERValue)
    , _SubscriptionStateLeave_Abandoned :: !Bool
    }
    deriving (Show, Eq, Ord)

data SubscriptionNotification
    = Subscription_Files !SubscriptionFiles
    | Subscription_StateEnter SubscriptionStateEnter
    | Subscription_StateLeave SubscriptionStateLeave
    deriving (Show, Eq, Ord)

parseSubscriptionNotification :: [FileFieldLabel] -> BSERValue -> Parser SubscriptionNotification
parseSubscriptionNotification fileFieldLabels v@(BSERObject o) = do
    case M.lookup "files" o of
        Just _ -> do
            f <- parseSubscriptionFiles fileFieldLabels v
            pure $ Subscription_Files f
        Nothing ->
            case M.lookup "state-enter" o of
                Just _ -> do
                    s <- parseSubscriptionStateEnter v
                    pure $ Subscription_StateEnter s
                Nothing ->
                    case M.lookup "state-leave" o of
                        Just _ -> do
                            s <- parseSubscriptionStateLeave v
                            pure $ Subscription_StateLeave s
                        Nothing ->
                            fail "Unrecognized subscription notification"
parseSubscriptionNotification _ _ = fail "Not an Object"

parseSubscriptionFiles :: [FileFieldLabel] -> BSERValue -> Parser SubscriptionFiles
parseSubscriptionFiles fileFieldLabels (BSERObject o) = do
    clockId <- parseClockId o
    root <- o .: "root"
    subscription <- o .: "subscription"
    files <- o .: "files"
    files' <- mapM (parseFileFields fileFieldLabels) files
    isFreshInstance <- o .: "is_fresh_instance"
    pure SubscriptionFiles
        { _SubscriptionFiles_Clock = clockId
        , _SubscriptionFiles_Root = root
        , _SubscriptionFiles_Subscription = SubscriptionName subscription
        , _SubscriptionFiles_Files = files'
        , _SubscriptionFiles_IsFreshInstance = isFreshInstance
        }
parseSubscriptionFiles _ _ = fail "Not an Object"

parseSubscriptionStateEnter :: BSERValue -> Parser SubscriptionStateEnter
parseSubscriptionStateEnter (BSERObject o) = do
    clockId <- parseClockId o
    root <- o .: "root"
    subscription <- o .: "subscription"
    state <- o .: "state-enter"
    let metadata = case M.lookup "metadata" o of
            Nothing -> Nothing
            Just v -> Just v
    pure SubscriptionStateEnter
      { _SubscriptionStateEnter_Clock = clockId
      , _SubscriptionStateEnter_Root = root
      , _SubscriptionStateEnter_Subscription = SubscriptionName subscription
      , _SubscriptionStateEnter_State = StateName state
      , _SubscriptionStateEnter_Metadata = metadata
      }
parseSubscriptionStateEnter _ = fail "Not an Object"

parseSubscriptionStateLeave :: BSERValue -> Parser SubscriptionStateLeave
parseSubscriptionStateLeave (BSERObject o) = do
    clockId <- parseClockId o
    root <- o .: "root"
    subscription <- o .: "subscription"
    state <- o .: "state-leave"
    let metadata = case M.lookup "metadata" o of
            Nothing -> Nothing
            Just v -> Just v
    let abandoned = case M.lookup "abandoned" o of
            Just (BSERBool True) -> True
            _ -> False
    pure SubscriptionStateLeave
      { _SubscriptionStateLeave_Clock = clockId
      , _SubscriptionStateLeave_Root = root
      , _SubscriptionStateLeave_Subscription = SubscriptionName subscription
      , _SubscriptionStateLeave_State = StateName state
      , _SubscriptionStateLeave_Metadata = metadata
      , _SubscriptionStateLeave_Abandoned = abandoned
      }
parseSubscriptionStateLeave _ = fail "Not an Object"

parseClockId :: BSERObject -> Parser ClockId
parseClockId o = do
    clockId <- o .: "clock"
    unless ("c:" `BC8.isPrefixOf` clockId) $
        fail $ "Invalid clock id: " ++ BC8.unpack clockId
    pure (ClockId clockId)
