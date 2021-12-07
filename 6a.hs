{-# LANGUAGE NoImplicitPrelude #-}

import AOC

main :: IO ()
main = interact' $ f . map (read :: String -> Int) . splitOn ","

f :: [Int] -> Int
f = length . (!! 80) . iterate progressDay

progressDay :: [Int] -> [Int]
progressDay xs = do
  x <- xs 
  if x == 0
    then
      [6,8]
    else 
      pure $ x - 1

