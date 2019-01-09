{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LinearTypes #-}
module Data.UFW where

import qualified Data.Vector        as Vec
import Data.Vector                  (Vector, (!), (//))
import Control.Monad.State.Strict


-- 1,2,3 testing
frugal :: a ⊸ (a,a)
frugal a = (a,a)

data UFWState = UFWState
   { _parent :: Vector Int
   , _size   :: Vector Int
   , _count  :: Int
   }

newtype UFW a = UFW { unwrapUFW :: State UFWState a }
   deriving (Functor, Applicative, Monad, MonadState UFWState)

mkUFW :: Int -> UFWState
mkUFW n = UFWState
   { _parent = Vec.fromList [0..n-1]
   , _size   = Vec.fromList [0..n-1]
   , _count  = n
   }

runUFW :: Int -> UFW a -> a
runUFW n ufw = retVal
   where (retVal, _) = runState (unwrapUFW ufw) (mkUFW n)

getCount :: UFW Int
getCount = gets _count

find :: Int -> UFW Int
find p = do
   parentOfP <- (! p) <$> gets _parent
   if parentOfP /= p
      then find parentOfP
      else return p

connected :: Int -> Int -> UFW Bool
connected p q = do
   rootP <- find p
   rootQ <- find q
   return $ rootP == rootQ

union :: Int -> Int -> UFW ()
union p q = do
   rootP <- find p
   rootQ <- find q
   unless (rootP == rootQ) $ do
      qSize <- (! rootQ) <$> gets _size
      pSize <- (! rootP) <$> gets _size
      if pSize < qSize
         then do
            modifyParent (// [(rootP,rootQ)])
            modifySize   (// [(rootQ, pSize+qSize)])
         else do
            modifyParent (// [(rootQ,rootP)])
            modifySize   (// [(rootP, pSize+qSize)])
      modifyCount (subtract 1)

modifyCount :: (Int -> Int) -> UFW ()
modifyCount f = do
   state <- get
   put $ state { _count = f (_count state)}

modifyParent :: (Vector Int -> Vector Int) -> UFW ()
modifyParent f = do
   state <- get
   put $ state { _parent = f (_parent state)}

modifySize :: (Vector Int -> Vector Int) -> UFW ()
modifySize f = do
   state <- get
   put $ state { _size = f (_size state)}
