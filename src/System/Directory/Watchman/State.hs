{-# LANGUAGE OverloadedStrings #-}

module System.Directory.Watchman.State
    ( StateName(..)
    , StateParams
    , renderStateEnter
    , renderStateLeave

    , setMetadata
    ) where

import Data.Foldable (foldl')
import Data.ByteString (ByteString)
import qualified Data.Sequence as Seq
import qualified Data.Map as M

import System.Directory.Watchman.BSER
import System.Directory.Watchman.SyncTimeout
import System.Directory.Watchman.WFilePath

newtype StateName = StateName ByteString
    deriving (Show, Eq, Ord)

data StateParams = StateParams
    { _StateParams_SyncTimeout :: !(Maybe Int)
    , _StateParams_Metadata :: !(Maybe BSERValue)
    }

defaultStateParams :: StateParams
defaultStateParams = StateParams
    { _StateParams_SyncTimeout = Nothing
    , _StateParams_Metadata = Nothing
    }

instance HasSyncTimeoutOption StateParams where
    setSyncTimeout n x = x { _StateParams_SyncTimeout = n }

setMetadata :: BSERValue -> (StateParams -> StateParams)
setMetadata v x = x { _StateParams_Metadata = Just v }

renderStateEnter :: WFilePath -> StateName -> [StateParams -> StateParams] -> BSERValue
renderStateEnter (WFilePath filepath) (StateName stateName) params =
    case params' of
        StateParams Nothing Nothing -> BSERArray $ Seq.fromList
            [ BSERString "state-enter"
            , BSERString filepath
            , BSERString stateName
            ]
        _ -> BSERArray $ Seq.fromList
            [ BSERString "state-enter"
            , BSERString filepath
            , renderStateParams params'
            ]
    where
    params' = applyStateParams params

renderStateLeave :: WFilePath -> StateName -> [StateParams -> StateParams] -> BSERValue
renderStateLeave (WFilePath filepath) (StateName stateName) params =
    case params' of
        StateParams Nothing Nothing -> BSERArray $ Seq.fromList
            [ BSERString "state-leave"
            , BSERString filepath
            , BSERString stateName
            ]
        _ -> BSERArray $ Seq.fromList
            [ BSERString "state-leave"
            , BSERString filepath
            , renderStateParams params'
            ]
    where
    params' = applyStateParams params

applyStateParams :: [StateParams -> StateParams] -> StateParams
applyStateParams = foldl' (\x f -> f x) defaultStateParams

renderStateParams :: StateParams -> BSERValue
renderStateParams params = BSERObject $ M.unions
    [ case _StateParams_SyncTimeout params of
        Nothing -> M.empty
        Just n -> M.singleton "sync_timeout" (compactBSERInt n)
    , case _StateParams_Metadata params of
        Nothing -> M.empty
        Just v -> M.singleton "metadata" v
    ]
