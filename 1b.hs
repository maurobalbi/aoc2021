{-# LANGUAGE NoImplicitPrelude #-}

import AOC

main :: IO ()
main = interact $ show . f . window . map (read :: String -> Int) 

f :: [Int] -> Int
f xs = count True $ zipWith (<) xs (tail xs)

window :: [Int] -> [Int]
window xs = zipWith3 (\x y z -> x + y + z) xs (tail xs) (tail $ tail xs)