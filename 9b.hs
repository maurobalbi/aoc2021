{-# LANGUAGE NoImplicitPrelude #-}

import AOC hiding (concatMap, lookup)

import Data.MultiSet hiding (map, filter)
import qualified Data.Map as M

main :: IO ()
main = interact $ f . map (map (read . (: "")) :: String -> [Int])

f xss = foldr (\x acc -> snd x * acc) 1 $ take 3 . sortBySize $  toOccurList . findLowestPoints $ points 
  where
    findLowestPoints x = f' (M.fromList x) $  fromList $ filter (\x -> snd x < 9) x 
    sortBySize = sortBy (\(_, t) (_, u) -> u `compare` t)
    points = pointTuple . zip [0 ..] $ zip [0 ..] <$> xss

f' :: M.Map (Int,Int) Int -> MultiSet ((Int, Int), Int) -> MultiSet ((Int, Int), Int)
f' xss = concatMap (\a -> [pointsTo xss a])

pointTuple :: Monad m => m (b1, m (a, b2)) -> m ((a, b1), b2)
pointTuple xss = do
  (y, row) <- xss
  (x, nr) <- row
  pure ((x,y), nr)

pointsTo :: M.Map (Int,Int) Int -> ((Int,Int), Int) -> ((Int, Int), Int)
pointsTo m p@((x,y), v) = if smallestNeighbour == p then p else pointsTo m smallestNeighbour
  where 
        smallestNeighbour =  minimumBy (\((x1,y1), v1) ((x2,y2),v2) -> v1 `compare` v2) [((x,y),v), ((x,y-1),top), ((x, y+1), bottom), ((x-1,y), left), ((x+1,y), right)]
        top = fromMaybe 9 $ M.lookup (x,y - 1) m
        bottom = fromMaybe 9 $ M.lookup (x, y + 1) m
        left = fromMaybe 9 $ M.lookup (x - 1, y) m 
        right = fromMaybe 9 $ M.lookup (x + 1, y) m



