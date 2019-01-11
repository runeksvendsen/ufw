{-# LANGUAGE FlexibleContexts #-}
module UFW.Parse
( strIntPair
, strLinePairs
, Char8.ByteString
, interact
)
where

import Prelude                                  hiding (interact)
import qualified Data.ByteString.Lazy.Char8     as Char8
import Data.Maybe                               (fromMaybe)


interact :: (Char8.ByteString -> String) -> IO ()
interact f = Char8.interact (Char8.pack . f)

-- | Decode input of the form "%d %d"
strIntPair :: Char8.ByteString -> (Int,Int)
strIntPair lineStr =
   let (p,q) = Char8.span (/= ' ') lineStr
       readIntOrFail bs = fst . fromMaybe (error $ "Bad input: " ++ show bs) $ Char8.readInt bs
   in (readIntOrFail p, readIntOrFail $ Char8.drop 1 q)

-- | Decode input of the form "%d %d\n%d %d\n ..."
strLinePairs :: Char8.ByteString -> [(Int,Int)]
strLinePairs = map strIntPair . Char8.lines
