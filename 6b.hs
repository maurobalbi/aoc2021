{-# LANGUAGE NoImplicitPrelude #-}

import AOC hiding (concatMap)
import Data.MultiSet (MultiSet, concatMap, fromList, size)

main :: IO ()
main = interact' $ f . map (read :: String -> Int) . splitOn ","

f :: [Int] -> Int
f = size . simulate . fromList
  where
    simulate = (!! 256) . iterate progressDay

progressDay :: MultiSet Int -> MultiSet Int
progressDay = concatMap (\a -> if a == 0 then [6, 8] else [a - 1])
