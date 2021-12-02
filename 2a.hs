{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.ByteString.Builder (integerDec)

main :: IO ()
main = interact $ f . parseList p

data Direction = Up | Down | Forward deriving (Show, Eq, Read, Ord, Bounded, Enum)

p :: Parser (Direction, Int)
p = do
  d <- enump
  char ' '
  n <- integer
  pure (d, n)

f :: Foldable t => t (Direction, Int) -> Int
f xs = x * y
  where
    (x, y) = foldr move (0, 0) xs

move :: Num a => (Direction, a) -> (a, a) -> (a, a)
move (Up, i) (x, y) = (x - i, y)
move (Down, i) (x, y) = (x + i, y)
move (Forward, i) (x, y) = (x, y + i)
