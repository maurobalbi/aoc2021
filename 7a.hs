{-# LANGUAGE NoImplicitPrelude #-}

import AOC

main :: IO ()
main = interact' $ f . map (read :: String -> Int) . splitOn ","

f :: [Int] -> Int
f xss = minimum $ map fuel [minimum xss .. maximum xss]
  where
    fuel n = foldr (\x acc -> acc + abs (x - n)) 0 xss
