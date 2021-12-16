{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

import AOC

import Debug.Trace
import qualified Data.Map as M
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP
import Text.Parsec.Char (upper)
import Text.ParserCombinators.Parsec.Token (GenTokenParser(squares))

main :: IO ()
main = interact $ run  . map (map (read @Int . (: "")) :: String -> [Int])

type Distance = Int

tupleMap :: [[Int]] -> M.Map (Int, Int) Distance
tupleMap xss = M.fromList $ do
  (y, row) <- coordinates
  (x, nr) <- row
  pure ((x, y), nr)
  where
    coordinates = zip [0 ..] $ zip [0 ..] <$> xss

genGraph :: [(Int, Int)] -> [(Int,Int,Int)] -> Gr Int Int
genGraph = mkGraph

createNode :: Int -> (Int,Int) -> (Int,Int)
createNode i (x,y) = (i*x + y, i * x + y)

createEdge :: Int -> M.Map (Int,Int) Int -> (Int,Int) -> [(Int, Int, Int)]
createEdge i m loc@(x,y) = catMaybes $ createEdge' <$> neighbours loc
  where
    createEdge' n = (nodeId loc, nodeId n,) <$> M.lookup n m
    nodeId = fst . createNode i
    neighbours (x,y) = [(x + xn, y + yn) | xn <- [-1..1], yn <- [-1..1], xn /= yn && xn /= (-1) * yn]

run x = spLength 0 (length cave * length cave - 1) graph
  where
    graph = genGraph nodes edges
    nodes = createNode (length cave) <$> M.keys map
    edges = createEdge (length cave) map =<< M.keys map
    map = tupleMap cave
    cave = increaseCave 5 x

increaseCave :: Int -> [[Int]] -> [[Int]]
increaseCave n list = repeatListWith wrapInc n <$> repeatListWith (wrapInc <$>) n list

repeatListWith :: (a -> a) -> Int -> [a] -> [a]
repeatListWith f 0 a = []
repeatListWith f n a = a ++ (f <$> repeatListWith f (n-1) a)

wrapInc :: Int -> Int
wrapInc x = if x == 9 then 1 else x + 1

