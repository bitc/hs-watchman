{-# LANGUAGE OverloadedStrings #-}

module System.Directory.Watchman.Clockspec
    ( Clockspec
    , ClockId(..)
    , renderClockspec
    , mkNamedCursor
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8

import System.Directory.Watchman.BSER

newtype NamedCursor = NamedCursor ByteString
    deriving (Show, Eq, Ord)

mkNamedCursor :: ByteString -> NamedCursor
mkNamedCursor name
    | "n:" `BC8.isPrefixOf` name = NamedCursor name
    | otherwise = error "Named Cursor must begin with \"n:\""

newtype ClockId = ClockId ByteString
    deriving (Show, Eq, Ord)

data Clockspec
    = Clockspec_Epoch Int -- TODO Should this not be Int64 or Double?
    | Clockspec_Cursor NamedCursor
    | Clockspec_ClockId ClockId
    deriving (Show, Eq, Ord)

renderClockspec :: Clockspec -> BSERValue
renderClockspec (Clockspec_Epoch n) = compactBSERInt n
renderClockspec (Clockspec_Cursor (NamedCursor s)) = BSERString s
renderClockspec (Clockspec_ClockId (ClockId s)) = BSERString s
