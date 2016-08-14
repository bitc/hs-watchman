module System.Directory.Watchman.WatchmanException
    ( WatchmanException(..)
    ) where

import Control.Exception (Exception, IOException)

import System.Directory.Watchman.Subscribe

data WatchmanException
    = WatchmanException_ErrorResponse String
    | WatchmanException_SockError IOException
    | WatchmanException_SubscriptionExists SubscriptionName
    deriving (Show, Eq)

instance Exception WatchmanException
