module Main where

import Prelude                            hiding (span, drop, lines, interact, read)
import qualified Data.UFW as UFW
import Control.Monad
import Data.ByteString.Lazy.Char8         ( ByteString
                                          , span, drop, lines, interact, readInt, pack
                                          )
import Data.Maybe                         (fromMaybe)


strIntPair :: ByteString -> (Int,Int)
strIntPair lineStr =
   let (p,q) = span (/= ' ') lineStr
       read bs = fst . fromMaybe (error $ "Bad input: " ++ show bs) $ readInt bs
   in (read p, read $ drop 1 q)

strLinePairs :: ByteString -> [(Int,Int)]
strLinePairs = map strIntPair . lines

main :: IO ()
main = do
   n <- readLn
   interact $ \inputStr ->
      let compCount = UFW.numComponents n (strLinePairs inputStr)
      in pack $ show compCount ++ " components\n"
