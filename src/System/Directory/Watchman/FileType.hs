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
    | TUnknown
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

fileTypeFromChar :: ByteString -> FileType
fileTypeFromChar "b" = TBlockSpecialFile
fileTypeFromChar "c" = TCharacterSpecialFile
fileTypeFromChar "d" = TDirectory
fileTypeFromChar "f" = TRegularFile
fileTypeFromChar "p" = TNamedPipe
fileTypeFromChar "l" = TSymbolicLink
fileTypeFromChar "s" = TSocket
fileTypeFromChar "D" = TSolarisDoor
fileTypeFromChar _ = TUnknown
