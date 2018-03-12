{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Data.UFW where

import qualified Data.Vector        as Vec
import Data.Vector                  (Vector, (!), (//))
import Control.Monad.State.Strict
import Control.Lens.TH
import Control.Lens.Setter
import Control.Lens.Getter


data UFWState = UFWState
   { _parent :: Vector Int
   , _size   :: Vector Int
   , _count  :: Int
   }

$(makeLenses ''UFWState)

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
   parent <- gets _parent
   if p /= parent ! p
      then find (parent ! p)
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
      qSize <- uses size (! rootQ)
      pSize <- uses size (! rootP)
      if pSize < qSize
         then do
            parent %= (// [(rootP,rootQ)])
            size   %= (// [(rootQ, pSize+qSize)])
         else do
            parent %= (// [(rootQ,rootP)])
            size   %= (// [(rootP, pSize+qSize)])
      count %= subtract 1
