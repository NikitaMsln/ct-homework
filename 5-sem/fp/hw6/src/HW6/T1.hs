module HW6.T1
  ( BucketsArray
  , CHT (..)
  , newCHT
  , getCHT
  , putCHT
  , sizeCHT
  , initCapacity
  , loadFactor
  ) where

import Control.Concurrent.Classy (STM, atomically)
import Control.Concurrent.Classy.STM (TArray, TVar, newTVar, readTVar, writeTVar)
import Control.Monad.Conc.Class (MonadConc)
import Data.Array.MArray (MArray (..), getBounds, getElems, newArray, newListArray, readArray,
                          writeArray)
import Data.Hashable (Hashable, hash)
import Data.List (find, findIndex)

initCapacity :: Int
initCapacity = 16

loadFactor :: Double
loadFactor = 0.75

type Bucket k v = [(k, v)]
type BucketsArray stm k v = TArray stm Int (Bucket k v)

data CHT stm k v = CHT
  { chtBuckets :: TVar stm (BucketsArray stm k v)
  , chtSize    :: TVar stm Int
  }

newCHT :: MonadConc m => m (CHT (STM m) k v)
newCHT = do
  size <- atomically $ newTVar 0
  array <- atomically $ newArray (0, initCapacity - 1) []
  varArray <- atomically $ newTVar array
  return CHT { chtBuckets = varArray, chtSize = size }

findBacketElement :: Eq k => k -> Bucket k v -> Maybe v
findBacketElement key = fmap snd . find (\(k, _) -> k == key)

getCHT :: (MonadConc m, Hashable k) => k -> CHT (STM m) k v -> m (Maybe v)
getCHT key cht = do
  array <- atomically $ readTVar (chtBuckets cht)
  (l, r) <- atomically $ getBounds array
  bucket <- atomically $ readArray array (hash key `mod` (r - l + 1) + l)
  return $ findBacketElement key bucket

setAt :: [a] -> Int -> a -> [a]
setAt xs i x = take i xs ++ [x] ++ drop (i + 1) xs

hashGroup :: Hashable k => Int -> [(k, v)] -> [[(k, v)]]
hashGroup size = foldr (\(k, v) l -> setAt l (hash k `mod` size) $ (k, v) : (l !! (hash k `mod` size))) (replicate size [])

putCHT :: (MonadConc m, Hashable k) => k -> v -> CHT (STM m) k v -> m ()
putCHT key val cht = atomically $ do
  size <- readTVar (chtSize cht)
  array <- readTVar (chtBuckets cht)
  (l, r) <- getBounds array
  bucket <- readArray array (hash key `mod` (r - l + 1) + l)
  case findIndex (\(k, _) -> k == key) bucket of
    Just index -> writeArray array (hash key `mod` (r - l + 1) + l) $ setAt bucket index (key, val)
    Nothing -> do
      writeTVar (chtSize cht) (size + 1)
      writeArray array (hash key `mod` (r - l + 1 + l)) $ (key, val) : bucket
      if (toRational $ r - l + 1) * toRational loadFactor <= toRational (size + 1) then do
        let newR = l + (r - l) * 2
        elems <- hashGroup (newR - l + 1) . foldMap id <$> getElems array
        writeTVar (chtBuckets cht) =<< newListArray (l, newR) elems
      else return ()

sizeCHT :: MonadConc m => CHT (STM m) k v -> m Int
sizeCHT cht = atomically $ readTVar (chtSize cht)
