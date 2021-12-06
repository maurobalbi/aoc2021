{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Debug.Trace
import qualified Prelude as P

main :: IO ()
main = interact' $ f . map (read :: String -> Int) . splitOn ","

f :: [Int] -> [Int]
f = id
