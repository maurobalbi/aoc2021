{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.Map hiding (foldl', filter, foldr)

main :: IO ()
main = interact $ f . parseList p

data Line = Line {start:: (Int, Int), end :: (Int, Int)} deriving (Show, Eq)

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

pointsFromLine :: Line -> [(Int, Int)]
pointsFromLine (Line (x1, y1) (x2, y2)) = [(x, y) | x <- pX, y <- pY, (x1 == x2 || y1 == y2) || x - x1 == y - y1 || x - x1 == y1 - y]
  where
    minX = min x1 x2
    maxX = max x1 x2
    minY = min y1 y2
    maxY = max y1 y2
    pX = [minX .. maxX]
    pY = [minY .. maxY]

f xs = length $ filter (\x -> 1 < snd x ) $ toList $ fromListWith (+) $ [(p, 1) | p <- points xs]
  where 
    points = foldr (\x acc -> pointsFromLine x ++ acc) []
--  length $ filter (\x -> 1 < snd x )
-- toList $ fromListWith (+) 
-- , x - x1 == y - y1 || x - x1 == (y - y1) * (-1)
--  length $ filter (\x -> 1 < snd x ) $ 
