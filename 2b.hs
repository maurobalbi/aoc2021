{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

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
    (x, y) = evalState (foldM move (0, 0) xs) 0


-- Making it a bit more complicated than necessary to get the hang of MonadState. Alternative solution of 2a with (,,)
move :: (Num a, MonadState a m) =>  (a, a) -> (Direction, a) -> m (a, a)
move (x, y) (Up, i) = do
  modify (\x -> x - i)
  pure (x, y)
move (x, y) (Down, i) = do
  modify (+ i)
  pure (x, y)
move (x, y) (Forward, i) = do
  aim <- get
  pure (x + i, y + i * aim)


