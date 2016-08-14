{-# LANGUAGE OverloadedStrings #-}

module System.Directory.Watchman.Fields
    ( FileField(..)
    , FileFieldLabel(..)
    , renderFieldLabels
    , parseFileFields
    ) where

import Control.Monad (forM)
import System.Directory.Watchman.WFilePath
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq

import System.Directory.Watchman.FileType
import System.Directory.Watchman.BSER
import System.Directory.Watchman.BSER.Parser

data FileField
    = Fname WFilePath -- ^ the filename, relative to the watched root
    | Fexists Bool -- ^ true if the file exists, false if it has been deleted
    | Fcclock ByteString -- ^ the "created clock"; the clock value when we first observed the file, or the clock value when it last switched from @not exists@ to @exists@.
    | Foclock ByteString -- ^ the "observed clock"; the clock value where we last observed some change in this file or its metadata.
    | Fctime Int -- ^ last inode change time measured in integer seconds
    | Fctime_ms Int -- ^ last inode change time measured in integer milliseconds
    | Fctime_us Int64 -- ^ last inode change time measured in integer microseconds
    | Fctime_ns Int64 -- ^ last inode change time measured in integer nanoseconds
    | Fctime_f Double -- ^ last inode change time measured in floating point seconds
    | Fmtime Int -- ^ modified time measured in integer seconds
    | Fmtime_ms Int -- ^ modified time measured in integer milliseconds
    | Fmtime_us Int64 -- ^ modified time measured in integer microseconds
    | Fmtime_ns Int64 -- ^ modified time measured in integer nanoseconds
    | Fmtime_f Double -- ^ modified time measured in floating point seconds
    | Fsize Int64 -- ^ file size in bytes
    | Fmode Int -- ^ file (or directory) mode expressed as a decimal integer
    | Fuid Int -- ^ the owning uid
    | Fgid Int -- ^ the owning gid
    | Fino Int -- ^ the inode number
    | Fdev Int -- ^ the device number
    | Fnlink Int -- ^ number of hard links
    | Fnew Bool -- ^ whether this entry is newer than the since generator criteria
    | Ftype FileType -- ^ the file type
    | Fsymlink_target (Maybe WFilePath) -- ^ the target of a symbolic link if the file is a symbolic link
    deriving (Show, Eq, Ord)

data FileFieldLabel
    = FLname -- ^ the filename, relative to the watched root
    | FLexists -- ^ true if the file exists, false if it has been deleted
    | FLcclock -- ^ the "created clock"; the clock value when we first observed the file, or the clock value when it last switched from !exists to exists.
    | FLoclock -- ^ the "observed clock"; the clock value where we last observed some change in this file or its metadata.
    | FLctime -- ^ last inode change time measured in integer seconds
    | FLctime_ms -- ^ last inode change time measured in integer milliseconds
    | FLctime_us -- ^ last inode change time measured in integer microseconds
    | FLctime_ns -- ^ last inode change time measured in integer nanoseconds
    | FLctime_f -- ^ last inode change time measured in floating point seconds
    | FLmtime -- ^ modified time measured in integer seconds
    | FLmtime_ms -- ^ modified time measured in integer milliseconds
    | FLmtime_us -- ^ modified time measured in integer microseconds
    | FLmtime_ns -- ^ modified time measured in integer nanoseconds
    | FLmtime_f -- ^ modified time measured in floating point seconds
    | FLsize -- ^ file size in bytes
    | FLmode -- ^ file (or directory) mode expressed as a decimal integer
    | FLuid -- ^ the owning uid
    | FLgid -- ^ the owning gid
    | FLino -- ^ the inode number
    | FLdev -- ^ the device number
    | FLnlink -- ^ number of hard links
    | FLnew -- ^ whether this entry is newer than the since generator criteria
    | FLtype -- ^ the file type
    | FLsymlink_target -- ^ the target of a symbolic link if the file is a symbolic link
    deriving (Show, Eq, Ord)


renderFileFieldLabel :: FileFieldLabel -> ByteString
renderFileFieldLabel FLname = "name"
renderFileFieldLabel FLexists = "exists"
renderFileFieldLabel FLcclock = "cclock"
renderFileFieldLabel FLoclock = "oclock"
renderFileFieldLabel FLctime = "ctime"
renderFileFieldLabel FLctime_ms = "ctime_ms"
renderFileFieldLabel FLctime_us = "ctime_us"
renderFileFieldLabel FLctime_ns = "ctime_ns"
renderFileFieldLabel FLctime_f = "ctime_f"
renderFileFieldLabel FLmtime = "mtime"
renderFileFieldLabel FLmtime_ms = "mtime_ms"
renderFileFieldLabel FLmtime_us = "mtime_us"
renderFileFieldLabel FLmtime_ns = "mtime_ns"
renderFileFieldLabel FLmtime_f = "mtime_f"
renderFileFieldLabel FLsize = "size"
renderFileFieldLabel FLmode = "mode"
renderFileFieldLabel FLuid = "uid"
renderFileFieldLabel FLgid = "gid"
renderFileFieldLabel FLino = "ino"
renderFileFieldLabel FLdev = "dev"
renderFileFieldLabel FLnlink = "nlink"
renderFileFieldLabel FLnew = "new"
renderFileFieldLabel FLtype = "type"
renderFileFieldLabel FLsymlink_target = "symlink_target"

parseFileField :: FileFieldLabel -> BSERValue -> Parser FileField
parseFileField FLname (BSERString s) = pure $ Fname (WFilePath s)
parseFileField FLname _ = fail "\"name\" field is not a string"
parseFileField FLexists (BSERBool b) = pure $ Fexists b
parseFileField FLexists _ = fail "\"exists\" field is not a boolean"
parseFileField FLcclock _ = error "TODO 32839423526"
parseFileField FLoclock _ = error "TODO 32839423526"
parseFileField FLctime int = case readBSERInt int of { Right n -> pure (Fctime n); Left err -> fail err }
parseFileField FLctime_ms int = case readBSERInt int of { Right n -> pure (Fctime_ms n); Left err -> fail err }
parseFileField FLctime_us int = case readBSERInt64 int of { Right n -> pure (Fctime_us n); Left err -> fail err }
parseFileField FLctime_ns int = case readBSERInt64 int of { Right n -> pure (Fctime_ns n); Left err -> fail err }
parseFileField FLctime_f (BSERReal r) = pure $ Fctime_f r
parseFileField FLctime_f _ = error "\"ctime_f\" field is not a real"
parseFileField FLmtime int = case readBSERInt int of { Right n -> pure (Fmtime n); Left err -> fail err }
parseFileField FLmtime_ms int = case readBSERInt int of { Right n -> pure (Fmtime_ms n); Left err -> fail err }
parseFileField FLmtime_us int = case readBSERInt64 int of { Right n -> pure (Fmtime_us n); Left err -> fail err }
parseFileField FLmtime_ns int = case readBSERInt64 int of { Right n -> pure (Fmtime_ns n); Left err -> fail err }
parseFileField FLmtime_f (BSERReal r) = pure $ Fmtime_f r
parseFileField FLmtime_f _ = error "\"mtime_f\" field is not a real"
parseFileField FLsize int = case readBSERInt64 int of { Right n -> pure (Fsize n); Left err -> fail err }
parseFileField FLmode int = case readBSERInt int of { Right n -> pure (Fmode n); Left err -> fail err }
parseFileField FLuid int = case readBSERInt int of { Right n -> pure (Fuid n); Left err -> fail err }
parseFileField FLgid int = case readBSERInt int of { Right n -> pure (Fgid n); Left err -> fail err }
parseFileField FLino int = case readBSERInt int of { Right n -> pure (Fino n); Left err -> fail err }
parseFileField FLdev int = case readBSERInt int of { Right n -> pure (Fdev n); Left err -> fail err }
parseFileField FLnlink int = case readBSERInt int of { Right n -> pure (Fnlink n); Left err -> fail err }
parseFileField FLnew (BSERBool b) = pure $ Fnew b
parseFileField FLnew _ = error "\"new\" field is not a boolean"
parseFileField FLtype (BSERString s) = case fileTypeFromChar s of { Just t -> pure (Ftype t); Nothing -> fail $ "Invalid file type: " ++ BC.unpack s}
parseFileField FLtype _ = error "\"type\" field is not a string"
parseFileField FLsymlink_target BSERNull = pure $ Fsymlink_target Nothing
parseFileField FLsymlink_target (BSERString s) = pure $ Fsymlink_target (Just (WFilePath s))
parseFileField FLsymlink_target _ = error "\"symlink_target\" field is not a string or null"

renderFieldLabels :: [FileFieldLabel] -> Map ByteString BSERValue
renderFieldLabels [] = error "Fields list is empty"
renderFieldLabels labels =
    M.singleton "fields" (BSERArray (fmap (BSERString . renderFileFieldLabel) (Seq.fromList labels)))

parseFileFields :: [FileFieldLabel] -> BSERValue -> Parser [FileField]
parseFileFields [single] val = parseFileField single val >>= pure . (:[])
parseFileFields fileFieldLabels (BSERObject o) = do
    forM fileFieldLabels $ \f -> do
        v <- o .: (renderFileFieldLabel f)
        parseFileField f v
parseFileFields _ _ = fail "Not an Object"
