{-# LANGUAGE NoImplicitPrelude #-}

import AOC

main :: IO ()
main = interact $ show . f . map (read :: String -> Int)  

f :: [Int] -> Int
f xs = count True $ zipWith (<) xs (tail xs)




