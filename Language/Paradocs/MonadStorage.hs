{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}

module Language.Paradocs.MonadStorage where

import           Prelude                            hiding (lookup)
import           Control.Applicative
import           Control.Monad.Reader
import           Control.Bool
import qualified Data.Hashable                      as Hashable
import qualified Data.HashMap.Strict                as HashMap
import qualified System.Directory               as Directory

class Monad m => MonadStorage m where
  maybeReadFile :: FilePath -> m (Maybe String)

instance MonadStorage IO where
  maybeReadFile path =
    ifThenElseM
      (Directory.doesFileExist path)
      (Just <$> readFile path)
      (return Nothing)

instance MonadStorage (HashMapStorage String String) where
  maybeReadFile = lookup

newtype HashMapStorage k v a
    = HashMapStorage
      {
        unHashMapStorage :: (Reader (HashMap.HashMap k v) a)
      }
    deriving (Functor, Applicative, Monad, MonadReader (HashMap.HashMap k v))

lookup :: (Eq k, Hashable.Hashable k) => k -> HashMapStorage k v (Maybe v)
lookup k = HashMap.lookup k <$> ask

runHashMapStorage :: forall k v a. (Eq k, Hashable.Hashable k) => HashMapStorage k v a -> HashMap.HashMap k v -> a
runHashMapStorage = runReader  . unHashMapStorage
