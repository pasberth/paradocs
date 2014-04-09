{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}

module Language.Paradocs.MonadStorage where

import           Prelude                            hiding (lookup)
import           Control.Applicative
--import           Control.Monad.Operational.Simple
import           Control.Monad.Identity
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

data HashMapStorageAction k v a where
  Lookup :: k -> HashMapStorageAction k v (Maybe v)

--newtype HashMapStorage k v a
--  = HashMapStorage (Program (HashMapStorageAction k v) a)
--  deriving (Functor, Monad, Operational (HashMapStorageAction k v))

newtype HashMapStorage k v a
  = HashMapStorage (Identity a)
  deriving (Functor, Monad, Applicative)

lookup :: (Eq k, Hashable.Hashable k) => k -> HashMapStorage k v (Maybe v)
--lookup = singleton . Lookup
lookup = undefined

runHashMapStorage :: forall k v a. (Eq k, Hashable.Hashable k) => HashMapStorage k v a -> HashMap.HashMap k v -> a
--runHashMapStorage (HashMapStorage program) storage = runIdentity $ interpret f program where
--  f :: forall x. HashMapStorageAction k v x -> Identity x
--  f (Lookup k) = return (HashMap.lookup k storage)
runHashMapStorage = undefined
