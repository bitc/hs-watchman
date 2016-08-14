module System.Directory.Watchman.BSER
    ( BSERObject
    , BSERValue(..)
    , compactBSERInt
    , readBSERInt
    , readBSERInt64
    ) where

import Control.Monad (replicateM, forM, forM_, when)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Int
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Sequence (Seq)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

type BSERObject = Map ByteString BSERValue

-- | See:
--   <https://facebook.github.io/watchman/docs/bser.html>
data BSERValue
    = BSERArray (Seq BSERValue)
    | BSERObject (Map ByteString BSERValue)
    | BSERString ByteString
    | BSERInt8 Int8
    | BSERInt16 Int16
    | BSERInt32 Int32
    | BSERInt64 Int64
    | BSERReal Double
    | BSERBool Bool
    | BSERNull
    deriving (Show, Eq, Ord)

readBSERInt :: BSERValue -> Either String Int
readBSERInt (BSERInt8 n) = Right (fromIntegral n)
readBSERInt (BSERInt16 n) = Right (fromIntegral n)
readBSERInt (BSERInt32 n) = Right (fromIntegral n)
readBSERInt (BSERInt64 n) =
    if n >= fromIntegral (minBound :: Int) && n <= fromIntegral (maxBound :: Int)
        then Right (fromIntegral n)
        else Left conversionError
    where
    conversionError = "Integer value is out of range"
readBSERInt _ = Left "Not an Integer"

readBSERInt64 :: BSERValue -> Either String Int64
readBSERInt64 (BSERInt8 n) = Right (fromIntegral n)
readBSERInt64 (BSERInt16 n) = Right (fromIntegral n)
readBSERInt64 (BSERInt32 n) = Right (fromIntegral n)
readBSERInt64 (BSERInt64 n) = Right n
readBSERInt64 _ = Left "Not an Integer"


-- | Chooses the smallest BSERInt* that the number will fit into
compactBSERInt :: Integral n => n -> BSERValue
compactBSERInt n | n < 0x80 && n >= (-0x80) = BSERInt8 (fromIntegral n)
                 | n < 0x8000 && n >= (-0x8000) = BSERInt16 (fromIntegral n)
                 | n < 0x80000000 && n >= (-0x80000000) = BSERInt32 (fromIntegral n)
                 | otherwise = BSERInt64 (fromIntegral n)

instance Binary BSERValue where
    put (BSERArray elements) = do
        putWord8 0x00
        put (compactBSERInt (Seq.length elements))
        mapM_ put elements
    put (BSERObject o) = do
        putWord8 0x01
        put (compactBSERInt (M.size o))
        forM_ (M.assocs o) $ \(k, v) -> do
            put (BSERString k)
            put v
    put (BSERString str) = do
        putWord8 0x02
        put (compactBSERInt (B.length str))
        putByteString str
    put (BSERInt8 n) = do
        putWord8 0x03
        putInt8 n
    put (BSERInt16 n) = do
        putWord8 0x04
        putInt16host n
    put (BSERInt32 n) = do
        putWord8 0x05
        putInt32host n
    put (BSERInt64 n) = do
        putWord8 0x06
        putInt64host n
    put (BSERReal r) = do
        putWord8 0x07
        putDoublehost r
    put (BSERBool True) =
        putWord8 0x08
    put (BSERBool False) =
        putWord8 0x09
    put BSERNull =
        putWord8 0x0a

    get = do
        tag <- getWord8
        case tag of
            0x00 -> do
                numVal <- get
                num <- case readBSERInt numVal of
                    Left err -> fail err
                    Right n -> pure n
                when (num < 0) $
                    fail "Negative Array length"
                elements <- replicateM num get
                pure (BSERArray (Seq.fromList elements))
            0x01 -> do
                numVal <- get
                num <- case readBSERInt numVal of
                    Left err -> fail err
                    Right n -> pure n
                when (num < 0) $
                    fail "Negative number of properties for Object"
                pairs <- replicateM num $ do
                    key <- get
                    val <- get
                    keyStr <- case key of
                        BSERString str -> pure str
                        _ -> fail "Invalid Key type"
                    pure (keyStr, val)
                pure (BSERObject (M.fromList pairs))
            0x02 -> do
                lenVal <- get
                len <- case readBSERInt lenVal of
                    Left err -> fail err
                    Right n -> pure n
                when (len < 0) $
                    fail "Negative String length"
                str <- getByteString len
                pure (BSERString str)
            0x03 -> getInt8 >>= pure . BSERInt8
            0x04 -> getInt16host >>= pure . BSERInt16
            0x05 -> getInt32host >>= pure . BSERInt32
            0x06 -> getInt64host >>= pure . BSERInt64
            0x07 -> do
                r <- getDoublehost
                pure (BSERReal r)
            0x08 -> pure (BSERBool True)
            0x09 -> pure (BSERBool False)
            0x0a -> pure BSERNull
            0x0b -> do
                h <- get
                keys <- case h of
                    BSERArray keys -> forM (toList keys) $ \v -> do
                        case v of
                            BSERString str -> pure str
                            _ -> fail "Array of Templated Object header has a key that is not a String"
                    _ -> fail "Array of Templated Object header is not an Array"
                let numKeys = length keys
                numV <- get
                numObjs <- case readBSERInt numV of
                    Left err -> fail err
                    Right n -> pure n
                when (numObjs < 0) $
                    fail "Invalid number of values in Array of Templated Object"
                objs <- replicateM numObjs $ do
                    vals <- replicateM numKeys $ do
                        getTemplatedObjectValue
                    pure (BSERObject (buildMap keys vals))
                pure (BSERArray (Seq.fromList objs))
            unknown -> fail $ "Unknown tag: " ++ show unknown

getTemplatedObjectValue :: Get (Maybe BSERValue)
getTemplatedObjectValue = do
    tag <- lookAhead getWord8
    if tag == 0x0c then pure Nothing
        else do
            v <- get
            pure (Just v)

buildMap :: Ord k => [k] -> [Maybe a] -> Map k a
buildMap keys vals = M.fromList validPairs
    where
    pairs = zip keys vals
    validPairs = mapMaybe choose pairs
    choose (_, Nothing) = Nothing
    choose (k, Just v) = Just (k, v)
