module Circuits.Internal.Util where

import qualified Data.Vector as V

binarySearch :: Ord a => a -> V.Vector a -> Maybe Int
binarySearch needle haystack = go 0 (V.length haystack) where
  go low high
    | low == high = Nothing
    | otherwise = let mid = (high + low) `div` 2
                  in case needle `compare` V.unsafeIndex haystack mid of
                       LT -> go 0 mid
                       EQ -> Just mid
                       GT -> go (mid + 1) high
