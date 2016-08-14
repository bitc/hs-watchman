module System.Directory.Watchman.SyncTimeout
    ( HasSyncTimeoutOption(..)

    , syncTimeout
    , syncOff
    ) where

class HasSyncTimeoutOption a where
    setSyncTimeout :: Maybe Int -> a -> a

-- | Set the timeout value for query synchronization. If the timeout passes, the command will return with an error.
syncTimeout
    :: HasSyncTimeoutOption a
    => Int -- ^ Time to wait in milliseconds
    -> (a -> a)
syncTimeout n = setSyncTimeout (Just n)

-- | Disable query synchronization for this command.
--
-- The query will be evaluated over the present view of the tree, which may lag behind the present state of the filesystem.
syncOff :: HasSyncTimeoutOption a => (a -> a)
syncOff = setSyncTimeout (Just 0)
