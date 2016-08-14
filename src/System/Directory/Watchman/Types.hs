{-# LANGUAGE OverloadedStrings #-}

module System.Directory.Watchman.Types
    ( WatchmanSockFile
    , WatchmanConnection(..)
    ) where

import Network.Socket

type WatchmanSockFile = FilePath

newtype WatchmanConnection = WatchmanConnection Socket
