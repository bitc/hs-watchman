{-# LANGUAGE OverloadedStrings #-}

module System.Directory.Watchman.FileType
    ( FileType(..)
    , fileTypeChar
    , fileTypeFromChar
    ) where

import Data.ByteString (ByteString)

data FileType
    = TBlockSpecialFile
    | TCharacterSpecialFile
    | TDirectory
    | TRegularFile
    | TNamedPipe
    | TSymbolicLink
    | TSocket
    | TSolarisDoor
    deriving (Show, Eq, Ord)

fileTypeChar :: FileType -> ByteString
fileTypeChar TBlockSpecialFile = "b"
fileTypeChar TCharacterSpecialFile = "c"
fileTypeChar TDirectory = "d"
fileTypeChar TRegularFile = "f"
fileTypeChar TNamedPipe = "p"
fileTypeChar TSymbolicLink = "l"
fileTypeChar TSocket = "s"
fileTypeChar TSolarisDoor = "D"

fileTypeFromChar :: ByteString -> Maybe FileType
fileTypeFromChar "b" = Just TBlockSpecialFile
fileTypeFromChar "c" = Just TCharacterSpecialFile
fileTypeFromChar "d" = Just TDirectory
fileTypeFromChar "f" = Just TRegularFile
fileTypeFromChar "p" = Just TNamedPipe
fileTypeFromChar "l" = Just TSymbolicLink
fileTypeFromChar "s" = Just TSocket
fileTypeFromChar "D" = Just TSolarisDoor
fileTypeFromChar _ = Nothing
