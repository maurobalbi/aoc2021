{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Data.MultiSet as M

main :: IO ()
main = interactg run

type Mark = (Int, Int)

data Fold = X Int | Y Int deriving (Show)

p :: Parser Mark
p = do
  x <- integer
  char ','
  y <- integer
  pure (x, y)

parseFold :: Parser Fold
parseFold = do
  string "fold along "
  parseDirection
  where
    parseDirection =
      X <$> (char 'x' *> char '=' *> integer)
        <|> (Y <$> (char 'y' *> char '=' *> integer))

run x = M.distinctSize $  foldOn (head folds) `M.map` M.fromList marks
  where
    folds = parseList parseFold $ (!! 1) x
    marks = parseList p $ head x

foldOn :: Fold -> Mark -> Mark
foldOn (X fx) m@(x, _) | x < fx = m
foldOn (Y fy) m@(_, y) | y < fy = m
foldOn (X fx) (x, y) = (x -2 * (x - fx), y)
foldOn (Y fy) (x, y) = (x, y -2 * (y - fy))
