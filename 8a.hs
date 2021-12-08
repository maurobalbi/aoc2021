{-# LANGUAGE NoImplicitPrelude, TypeFamilies, DataKinds #-}

import AOC
import GHC.Hs.Expr (HsSplice(XSplice), ApplicativeArg (XApplicativeArg))

main :: IO ()
main = interact $ f . parseInput

parseInput :: [[Char]] -> [String]
parseInput xs = do
  x <- xs
  t <- tail $ splitOn "|" x
  w <- words  t
  guard $ length w == 2 || length w == 3 || length w == 4 || length w == 7
  pure w

f :: [a] -> Int
f = length



