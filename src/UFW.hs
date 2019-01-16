{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module UFW
( numComponents
) where

import Control.Monad
import Data.STRef
import Data.Array.ST
import Control.Monad.ST
import Control.Monad.Reader


numComponents :: Word -> [(Int,Int)] -> Word
numComponents n unions =
   runUFW n $ forM_ unions (uncurry union) >> getCount

newtype UFW s a = UFW { unwrapUFW :: ReaderT (UFWState s) (ST s) a }
   deriving (Functor, Applicative, Monad, MonadReader (UFWState s))

data UFWState s = UFWState
   { _parent :: STUArray s Int Int
   , _size   :: STUArray s Int Int
   , _count  :: STRef s Word
   }

mkInitialState :: Word -> ST s (UFWState s)
mkInitialState n = UFWState
    <$> newListArray (0, arraySize-1) [0..arraySize-1]
    <*> newListArray (0, arraySize-1) [0..arraySize-1]
    <*> newSTRef n
  where
    arraySize = fromIntegral n

runUFW :: Word -> (forall s. UFW s a) -> a
runUFW n ufw = runST $ do
    initState <- mkInitialState n
    runReaderT (unwrapUFW ufw) initState

getCount :: UFW s Word
getCount = UFW . ReaderT $ \state -> readSTRef (_count state)

withCount :: (Word -> Word)
          -> UFW s ()
withCount f = do
    countRef <- asks _count
    UFW . lift $ modifySTRef countRef f

arrayRead
   :: (UFWState s -> STUArray s Int Int)  -- ^ Which array to read from
   -> Int                                 -- ^ Array index to read
   -> UFW s Int
arrayRead arrayFn index = do
    array <- asks arrayFn
    UFW . lift $ readArray array index

arrayWrite
   :: (UFWState s -> STUArray s Int Int)  -- ^ Which array to write to
   -> (Int, Int)                          -- ^ (index, value) to write
   -> UFW s ()
arrayWrite arrayFn (index, value) = do
    array <- asks arrayFn
    UFW . lift $ writeArray array index value

find :: Int
     -> UFW s Int
find p = do
   parentOfP <- _parent `arrayRead` p
   if parentOfP /= p
      then find parentOfP
      else return p

connected :: Int
          -> Int
          -> UFW s Bool
connected p q = do
   rootP <- find p
   rootQ <- find q
   return $ rootP == rootQ

union :: Int
      -> Int
      -> UFW s ()
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
