{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.UFW
( UFW
, runUFW
, connected
, union
, getCount
)

where

import Control.Monad
import Data.Primitive.MutVar
import Data.Primitive.Array
import Control.Monad.ST
import Control.Monad.Reader
import Control.Monad.Primitive


type UFW s = ReaderT (UFWState s) (ST s)

data UFWState s = UFWState
   { _parent :: MutableArray s Int
   , _size   :: MutableArray s Int
   , _count  :: MutVar s Word
   }

mkInitialState :: Word -> ST s (UFWState s)
mkInitialState n = UFWState
    <$> newListArray arraySize [0..arraySize-1]
    <*> newListArray arraySize [0..arraySize-1]
    <*> newMutVar n
  where
    arraySize = fromIntegral n
    -- Same as 'Data.Array.MArray.newListArray'
    newListArray :: (PrimMonad m, Integral v)
                 => Int
                 -> [v]
                 -> m (MutableArray (PrimState m) v)
    newListArray size values =
        newArray (fromIntegral size) 0 >>=
            \array -> initArray size values array >> return array
    -- Initialize an array of the given size with the given values
    initArray size values emptyArray =
        foldM_ (\array (idx, val) -> writeArray array idx val >> return array)
               emptyArray
               (zip [0..size-1] values)

runUFW :: Word -> (forall s. UFW s a) -> a
runUFW n ufw = runST $ do
    initState <- mkInitialState n
    runReaderT ufw initState

getCount :: UFW s Word
getCount = ReaderT $ \state -> readMutVar (_count state)

withCount :: (PrimMonad m, MonadReader (UFWState (PrimState m)) m)
          => (Word -> Word)
          -> m ()
withCount f = do
    countRef <- asks _count
    readMutVar countRef >>= \count -> writeMutVar countRef (f count)

arrayRead
   :: (PrimMonad m, MonadReader r m)
   => (r -> MutableArray (PrimState m) a) -- ^ Which array to read from
   -> Int                                 -- ^ Array index to read
   -> m a
arrayRead arrayFn index =
    (`readArray` index) =<< asks arrayFn

arrayWrite
   :: (PrimMonad m, MonadReader r m)
   => (r -> MutableArray (PrimState m) a) -- ^ Which array to write to
   -> (Int, a)                            -- ^ (index, value) to write
   -> m ()
arrayWrite arrayFn (index, value) =
    (\array -> writeArray array index value) =<< asks arrayFn

find :: (PrimMonad m, MonadReader (UFWState (PrimState m)) m)
     => Int
     -> m Int
find p = do
   parentOfP <- _parent `arrayRead` p
   if parentOfP /= p
      then find parentOfP
      else return p

connected :: (PrimMonad m, MonadReader (UFWState (PrimState m)) m)
          => Int
          -> Int
          -> m Bool
connected p q = do
   rootP <- find p
   rootQ <- find q
   return $ rootP == rootQ

union :: (PrimMonad m, MonadReader (UFWState (PrimState m)) m)
      => Int
      -> Int
      -> m ()
union p q = do
   rootP <- find p
   rootQ <- find q
   unless (rootP == rootQ) $ do
      qSize <- _size `arrayRead` rootQ
      pSize <- _size `arrayRead` rootP
      if pSize < qSize
         then do
            _parent `arrayWrite` (rootP,rootQ)
            _size   `arrayWrite` (rootQ, pSize+qSize)
         else do
            _parent `arrayWrite` (rootQ,rootP)
            _size   `arrayWrite` (rootP, pSize+qSize)
      withCount (subtract 1)
