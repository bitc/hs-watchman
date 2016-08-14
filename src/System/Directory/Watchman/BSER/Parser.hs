{-# LANGUAGE RankNTypes #-}

module System.Directory.Watchman.BSER.Parser
    ( FromBSER(..)
    , Parser
    , Result(..)
    , parse
    , (.:)
    ) where

import Data.Foldable (toList)
import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq)

import System.Directory.Watchman.BSER

data Result a
    = Error String
    | Success a
    deriving (Show, Eq)

type Failure r = String -> Result r
type Success a r = a -> Result r

newtype Parser a = Parser
    { runParser :: forall r. Failure r -> Success a r -> Result r
    }

instance Monad Parser where
    m >>= g = Parser $ \kf ks -> let ks' a = runParser (g a) kf ks
                                 in runParser m kf ks'
    {-# INLINE (>>=) #-}
    return a = Parser $ \_kf ks -> ks a
    {-# INLINE return #-}
    fail msg = Parser $ \kf _ks -> kf msg
    {-# INLINE fail #-}

instance Functor Parser where
    fmap f m = Parser $ \kf ks -> let ks' a = ks (f a)
                                  in runParser m kf ks'
    {-# INLINE fmap #-}

instance Applicative Parser where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = apP
    {-# INLINE (<*>) #-}

instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a b = Parser $ \kf ks -> let kf' _ = runParser b kf ks
                                   in runParser a kf' ks
    {-# INLINE mplus #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  a <- e
  return (b a)


class FromBSER a where
    parseBSER :: BSERValue -> Parser a

instance FromBSER BSERValue where
    parseBSER x = pure x

instance FromBSER ByteString where
    parseBSER (BSERString s) = pure s
    parseBSER _ = fail "Not a string"

instance FromBSER Bool where
    parseBSER (BSERBool b) = pure b
    parseBSER _ = fail "Not a boolean"

instance FromBSER a => FromBSER [a] where
    parseBSER (BSERArray a) = do
        elems <- mapM parseBSER a
        pure (toList elems)
    parseBSER _ = fail "Not an array"

instance FromBSER a => FromBSER (Seq a) where
    parseBSER (BSERArray a) = do
        mapM parseBSER a
    parseBSER _ = fail "Not an array"

-- | Run a 'Parser'.
parse :: (a -> Parser b) -> a -> Result b
parse m v = runParser (m v) Error Success
{-# INLINE parse #-}

(.:) :: (FromBSER a) => BSERObject -> ByteString -> Parser a
obj .: key = case M.lookup key obj of
                Nothing -> empty
                Just v  -> parseBSER v
