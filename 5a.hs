{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.Map hiding (filter, foldl', foldr)

main :: IO ()
main = interact $ f . parseList p

data Line = Line {start :: (Int, Int), end :: (Int, Int)} deriving (Show, Eq)

p :: Parser Line
p = do
  p1 <- point
  string " -> "
  Line p1 <$> point
  where
    point = do
      x <- integer
      char ','
      y <- integer
      pure (x, y)

isVerticalOrHorizontal :: Line -> Bool
isVerticalOrHorizontal (Line (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2

pointsFromLine :: Line -> [(Int, Int)]
pointsFromLine (Line (x1, y1) (x2, y2)) = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

f xs = length $ filter (\x -> 1 < snd x) $ toList $ fromListWith (+) [(p, 1) | p <- points $ filter isVerticalOrHorizontal xs]
  where
    points = foldr (\x acc -> pointsFromLine x ++ acc) []
