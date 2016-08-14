{-# LANGUAGE OverloadedStrings #-}

import System.FilePath
import System.IO.Temp
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Sequence as Seq

import System.Directory.Watchman.Fields
import System.Directory.Watchman.Query
import System.Directory.Watchman.Subscribe
import System.Directory.Watchman.WFilePath
import System.Directory.Watchman.WatchmanServer
import qualified System.Directory.Watchman as Watchman
import qualified System.Directory.Watchman.Expression as Expr

tests :: TestTree
tests = testGroup "Tests"
    [ testCase "test_watchmanServer" test_watchmanServer
    , testCase "test_version" test_version
    , testCase "test_watchList" test_watchList
    , testCase "test_query1" test_query1
    , testCase "test_subscribe" test_subscribe
    ]

main :: IO ()
main = defaultMain $ tests

test_watchmanServer :: Assertion
test_watchmanServer = withWatchmanServer Nothing (const $ pure ())

test_version :: Assertion
test_version = withWatchmanServer Nothing $ \sockFile -> do
    Watchman.WatchmanVersion v <- Watchman.version sockFile
    assertBool "Version string not empty" (not (null v))

test_watchList :: Assertion
test_watchList = withWatchmanServer Nothing $ \sockFile -> do
    withSystemTempDirectory "watchman_test" $ \tmpDir -> do
        let watchDir = WFilePath (BC8.pack tmpDir)
        _ <- Watchman.watch sockFile watchDir
        watchDirs <- Watchman.watchList sockFile
        watchDirs @?= [watchDir]

test_query1 :: Assertion
test_query1 = withWatchmanServer Nothing $ \sockFile -> do
    withSystemTempDirectory "watchman_test" $ \tmpDir -> do
        let watchDir = WFilePath (BC8.pack tmpDir)
        _ <- Watchman.watch sockFile watchDir
        BC8.writeFile (tmpDir </> "blah.txt") "blah"
        qr <- Watchman.query sockFile watchDir [] Expr.true [] [FLname]
        _QueryResult_Files qr @?= Seq.fromList [[Fname (WFilePath "blah.txt")]]
        pure ()

test_subscribe :: Assertion
test_subscribe = withWatchmanServer Nothing $ \sockFile -> do
    withSystemTempDirectory "watchman_test" $ \tmpDir -> do
        BC8.writeFile (tmpDir </> "foo.txt") "foo"
        let watchDir = WFilePath (BC8.pack tmpDir)
        _ <- Watchman.watch sockFile watchDir
        Watchman.withConnect sockFile $ \sock -> do
            subscription <- Watchman.subscribe sock watchDir (SubscriptionName "sub1") Expr.true [] [FLname, FLexists]
            initialMessage <- Watchman.readNotification subscription
            case initialMessage of
                Subscription_Files files -> do
                    _SubscriptionFiles_Files files @?= Seq.fromList [[Fname (WFilePath "foo.txt"), Fexists True]]
                x -> assertFailure $ "Unexpected notification: " ++ show x

            BC8.writeFile (tmpDir </> "blah.txt") "blah"
            notification <- Watchman.readNotification subscription
            case notification of
                Subscription_Files files -> do
                    _SubscriptionFiles_Files files @?= Seq.fromList [[Fname (WFilePath "blah.txt"), Fexists True]]
                x -> assertFailure $ "Unexpected notification: " ++ show x
