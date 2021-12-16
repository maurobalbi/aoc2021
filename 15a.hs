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

run x = spLength 0 9999 graph
  where
    graph = genGraph nodes edges
    nodes = createNode (length x) <$> M.keys (tupleMap x)
    edges = createEdge (length x) (tupleMap x) =<< M.keys (tupleMap x)


