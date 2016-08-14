module System.Directory.Watchman.WFilePath
    ( WFilePath(..)
    , toByteString
    ) where

import Data.ByteString (ByteString)

import System.Directory.Watchman.BSER.Parser

newtype WFilePath = WFilePath ByteString
    deriving (Show, Eq, Ord)

toByteString :: WFilePath -> ByteString
toByteString (WFilePath p) = p

instance FromBSER WFilePath where
    parseBSER x = do
        str <- parseBSER x
        pure (WFilePath str)
