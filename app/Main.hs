module Main where

import qualified Data.UFW as UFW
import Control.Monad


strIntPair :: String -> (Int,Int)
strIntPair lineStr =
   let (p,q) = span (/= ' ') lineStr
   in (read p, read $ drop 1 q)

strLinePairs :: String -> [(Int,Int)]
strLinePairs = map strIntPair . lines

main :: IO ()
main = do
   n <- readLn
   interact $ \inputStr -> UFW.runUFW n $ do
      forM_ (strLinePairs inputStr) $ \(p,q) -> do
         isConnected <- UFW.connected p q
         unless isConnected $ UFW.union p q
      compCount <- UFW.getCount
      return $ show compCount ++ " components\n"
