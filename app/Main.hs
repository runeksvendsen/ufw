module Main where

import qualified UFW.Parse
import qualified UFW


main :: IO ()
main = do
   n <- readLn
   UFW.Parse.interact $ \inputStr ->
      let compCount = UFW.numComponents n (UFW.Parse.strLinePairs inputStr)
      in show compCount ++ " components\n"
