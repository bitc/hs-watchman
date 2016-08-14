{-# LANGUAGE OverloadedStrings #-}

module System.Directory.Watchman.Query
    ( Generators
    , QueryParams
    , QueryResult(..)
    , renderQuery
    , parseQueryResult

    , since
    , suffix
    , path

    , relativeRoot
    , lockTimeout
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
import System.Directory.Watchman.SyncTimeout

data Since = Since Clockspec
data Suffix = Suffix [ByteString]
data Path = Path [(WFilePath, Maybe Int)]

-- | If none are set, then the default "all" generator is used
data Generators = Generators (Maybe Since) (Maybe Suffix) (Maybe Path)

since :: Clockspec -> (Generators -> Generators)
since clockSpec (Generators _ a b) = Generators (Just (Since clockSpec)) a b

suffix :: [ByteString] -> Generators -> Generators
suffix suffixes (Generators a _ b) = Generators a (Just (Suffix suffixes)) b

path :: [(WFilePath, Maybe Int)] -> Generators -> Generators
path paths (Generators a b _) = Generators a b (Just (Path paths))

relativeRoot :: WFilePath -> (QueryParams -> QueryParams)
relativeRoot p x = x { _QueryParams_RelativeRoot = Just p }

lockTimeout :: Int -> (QueryParams -> QueryParams)
lockTimeout n x = x { _QueryParams_LockTimeout = Just n }

applyGenerators :: [Generators -> Generators] -> Generators
applyGenerators = foldl' (\x f -> f x) (Generators Nothing Nothing Nothing)

applyParams :: [QueryParams -> QueryParams] -> QueryParams
applyParams = foldl' (\x f -> f x) (QueryParams Nothing Nothing Nothing)

data QueryParams = QueryParams
    { _QueryParams_RelativeRoot :: !(Maybe WFilePath)
    , _QueryParams_SyncTimeout :: !(Maybe Int)
    , _QueryParams_LockTimeout :: !(Maybe Int)
    }

instance HasSyncTimeoutOption QueryParams where
    setSyncTimeout n x = x { _QueryParams_SyncTimeout = n }

renderQuery :: WFilePath -> [Generators -> Generators] -> Expression -> [QueryParams -> QueryParams] -> [FileFieldLabel] -> BSERValue
renderQuery rootPath generators expr params fileFieldLabels =
    BSERArray $ Seq.fromList
        [ BSERString "query"
        , BSERString (toByteString rootPath)
        , BSERObject $ M.unions
            [ renderGenerators generators'
            , renderFieldLabels fileFieldLabels
            , M.singleton "expression" (renderExpression expr)
            , renderQueryParams params'
            ]
        ]
    where
    generators' = applyGenerators generators
    params' = applyParams params

renderGenerators :: Generators -> Map ByteString BSERValue
renderGenerators (Generators mbSince mbSuffix mbPath) =
    M.unions [renderedSince, renderedSuffix, renderedPath]
    where
    renderedSince = maybe
        M.empty
        (\(Since clockspec) -> M.singleton "since" (renderClockspec clockspec))
        mbSince
    renderedSuffix = maybe
        M.empty
        (\(Suffix suffixes) -> M.singleton "suffix" (BSERArray (fmap BSERString (Seq.fromList suffixes))))
        mbSuffix
    renderedPath = maybe
        M.empty
        (\(Path paths) -> M.singleton "path" (BSERArray (fmap renderPathElement (Seq.fromList paths))))
        mbPath
    renderPathElement :: (WFilePath, Maybe Int) -> BSERValue
    renderPathElement (path_, Nothing) = BSERString (toByteString path_)
    renderPathElement (path_, Just depth)
        | depth < 0 = error $ "Invalid depth value: " ++ show depth
        | otherwise =
            BSERObject $ M.fromList
              [ ("path", BSERString (toByteString path_))
              , ("depth", compactBSERInt depth)
              ]

renderQueryParams :: QueryParams -> Map ByteString BSERValue
renderQueryParams params =
    M.unions
        [ case _QueryParams_RelativeRoot params of
            Nothing -> M.empty
            Just (WFilePath p) -> M.singleton "relative_root" (BSERString p)
        , case _QueryParams_SyncTimeout params of
            Nothing -> M.empty
            Just n -> M.singleton "sync_timeout" (compactBSERInt n)
        , case _QueryParams_LockTimeout params of
            Nothing -> M.empty
            Just n -> M.singleton "lock_timeout" (compactBSERInt n)
        ]

data QueryResult = QueryResult
    { _QueryResult_Clock :: !ClockId
    , _QueryResult_Files :: !(Seq [FileField])
    , _QueryResult_IsFreshInstance :: !Bool
    }
    deriving (Show, Eq, Ord)

parseQueryResult :: [FileFieldLabel] -> BSERValue -> Parser QueryResult
parseQueryResult fileFieldLabels (BSERObject o) = do
    clockId <- o .: "clock"
    unless ("c:" `BC8.isPrefixOf` clockId) $
        fail $ "Invalid clock id: " ++ BC8.unpack clockId
    isFreshInstance <- o .: "is_fresh_instance"
    files <- o .: "files"
    files' <- mapM (parseFileFields fileFieldLabels) files
    pure QueryResult
        { _QueryResult_Clock = ClockId clockId
        , _QueryResult_Files = files'
        , _QueryResult_IsFreshInstance = isFreshInstance
        }
parseQueryResult _ _ = fail "Not an Object"
