{-# LANGUAGE MultiParamTypeClasses #-}
module Data.UnionFind.MonadRef (MonadRef(..)) where

import Control.Applicative
import Control.Monad.ST
import Data.IORef
import Data.STRef

class (Functor m, Applicative m, Monad m) => MonadRef m r where
  newRef :: a -> m (r a)
  readRef :: r a -> m a
  writeRef :: r a -> a -> m ()
  modifyRef :: r a -> (a -> a) -> m ()

instance MonadRef (ST s) (STRef s) where
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef
  modifyRef = modifySTRef

instance MonadRef IO IORef where
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef
  modifyRef = modifyIORef
