{-# LANGUAGE ScopedTypeVariables #-}

module System.Directory.Watchman.WatchmanServer
    ( WatchmanServerLaunchException(..)
    , withWatchmanServer
    , launchWatchman
    , shutdownWatchmanProcess
    ) where

import Control.Concurrent.Async
import System.FilePath
import Control.Exception (Exception, IOException, bracket, bracketOnError, try, throwIO)
import Data.Maybe (fromMaybe)
import System.IO.Temp
import System.Process
import System.Directory.Watchman.Types
import qualified System.Directory.Watchman as Watchman
import System.Directory.Watchman.WatchmanException
import Control.Concurrent (threadDelay)
import System.IO
import System.Exit (ExitCode)
import qualified Data.ByteString.Char8 as BC8

data WatchmanServerLaunchException
    = WatchmanServerLaunchException_ExecFailure IOException
    | WatchmanServerLaunchException_ConnectTimeout
    | WatchmanServerLaunchException_ProcessFailure ExitCode String
    deriving (Show)

instance Exception WatchmanServerLaunchException

withWatchmanServer :: Maybe FilePath -> (WatchmanSockFile -> IO a) -> IO a
withWatchmanServer mbWatchmanExe action =
    withSystemTempDirectory "hs_watchman" $ \tmpDir -> do
        bracket
            (launchWatchman watchmanExe tmpDir)
            shutdownWatchmanProcess
            (\(WatchmanServer _ sockFile _ _ _) -> action sockFile)
    where
    watchmanExe = fromMaybe "watchman" mbWatchmanExe


data WatchmanServer = WatchmanServer !ProcessHandle !WatchmanSockFile !Handle !Handle !Handle

launchWatchman :: FilePath -> FilePath -> IO WatchmanServer
launchWatchman watchmanExe tmpDir = do
    bracketOnError
        (launchWatchmanProcess watchmanExe tmpDir)
        terminateWatchmanProcess
        $ \ws@(WatchmanServer _ sockFile _ _ _) -> do
            withAsync (waitUntilRunningThread sockFile) $ \waitUntilRunningA -> do
                withAsync (checkProcessFailureThread ws) $ \startupErrorA -> do
                    _ <- waitAnyCancel [waitUntilRunningA, startupErrorA]
                    pure ws
    where
    watchmanConnectTimeoutMilliseconds = 10000
    checkRunningSnoozeMilliseconds = 4
    checkExitedSnoozeMilliseconds = 10
    waitUntilRunningThread sockFile = do
        withAsync (waitUntilWatchmanConnect sockFile) $ \connectA -> do
            withAsync (timeout watchmanConnectTimeoutMilliseconds WatchmanServerLaunchException_ConnectTimeout) $ \timeoutA -> do
                _ <- waitAnyCancel [connectA, timeoutA]
                pure ()
    waitUntilWatchmanConnect sockFile = do
        running <- watchmanIsRunning sockFile
        if running
            then pure ()
            else do
                threadDelay (checkRunningSnoozeMilliseconds * 1000)
                waitUntilWatchmanConnect sockFile
    checkProcessFailureThread ws@(WatchmanServer pid _ _ _ stderrH) = do
        mbExitCode <- getProcessExitCode pid
        case mbExitCode of
            Just exitCode -> do
                stderrText <- BC8.hGetContents stderrH
                throwIO $ WatchmanServerLaunchException_ProcessFailure exitCode (BC8.unpack stderrText)
            Nothing -> do
                threadDelay (checkExitedSnoozeMilliseconds * 1000)
                checkProcessFailureThread ws
    timeout milliseconds ex = do
        threadDelay (milliseconds * 1000)
        throwIO ex

watchmanIsRunning :: WatchmanSockFile -> IO Bool
watchmanIsRunning sockFile = do
    tryResult <- try $ Watchman.version sockFile
    case tryResult of
        Left (_ :: WatchmanException) -> pure False
        Right _ -> pure True

launchWatchmanProcess :: FilePath -> FilePath -> IO WatchmanServer
launchWatchmanProcess watchmanExe tmpDir = do
    tryResult <- try $ createProcess (proc watchmanExe args)
        { env = Just []
        , cwd = Just "/"
        , close_fds = True
        , std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
    case tryResult of
        Left ex -> throwIO $ WatchmanServerLaunchException_ExecFailure ex
        Right (Just stdinH, Just stdoutH, Just stderrH, processHandle) -> pure $ WatchmanServer processHandle sockFile stdinH stdoutH stderrH
        Right _ -> error "launchWatchmanProcess: The Impossible Happened"
    where
    sockFile = tmpDir </> "watchman.sock"
    args =
        [ "--sockname=" ++ sockFile
        , "--logfile=" ++ tmpDir </> "watchman.log"
        , "--pidfile=" ++ tmpDir </> "watchman.pid"
        , "--statefile=" ++ tmpDir </> "watchman.state"
        , "--no-save-state"
        , "--foreground"
        ]

terminateWatchmanProcess :: WatchmanServer -> IO ()
terminateWatchmanProcess (WatchmanServer processHandle _ stdinH stdoutH stderrH) = do
    terminateProcess processHandle
    _ <- waitForProcess processHandle
    hClose stdinH
    hClose stdoutH
    hClose stderrH

shutdownWatchmanProcess :: WatchmanServer -> IO ()
shutdownWatchmanProcess (WatchmanServer processHandle sockFile stdinH stdoutH stderrH) = do
    -- TODO If a timeout elapses, then force-kill the process with terminateProcess
    _ <- Watchman.shutdownServer sockFile
    _ <- waitForProcess processHandle
    hClose stdinH
    hClose stdoutH
    hClose stderrH
