{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.MemoTrie

main :: IO ()
main = interact' $ f . map (read :: String -> Int) . splitOn ","

f :: [Int] -> Int
f xss = minimum $ map fuel [minimum xss .. maximum xss]
  where
    fuel n = foldr (\x acc -> acc + faculty (abs (x - n))) 0 xss

faculty :: Int -> Int
faculty = memo $ \case
  0 -> 0
  n -> n + faculty (n - 1)
