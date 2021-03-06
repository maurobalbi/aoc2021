{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

import AOC
import qualified Data.Map as M
import Debug.Trace
import Data.Tuple
import Data.Bifunctor

data Node = Start | Big String | Small String | End deriving (Eq, Show, Ord)
type Graph = M.Map Node [Node]

main :: IO ()
main = interact $ run . parseList p

p :: Parser (Node, Node)
p = do
  i <- node
  char '-'
  o <- node
  pure (i,o)

node :: Parser Node
node = start <|> end <|> big <|> small
  where
    start = Start <$ try (string "start")
    end = End <$ try (string "end")
    big = Big <$> many1 upper
    small = Small <$> many1 lower

buildMap :: [(Node,Node)] -> M.Map Node [Node]
buildMap xs = M.fromListWith (++) $ second pure <$> (xs ++ (swap <$> xs))

run i = length $ routeS (buildMap i)

neighbours :: Ord k => M.Map k [a] -> k -> [a]
neighbours g n = fromMaybe [] $ M.lookup n g

routeS :: Graph -> [[Node]]
routeS g = go g [] Start
  where 
    go graph path from =
      case (from, M.lookup from graph) of
        (End, _) -> [End: path]
        (_, Nothing) -> []
        (Start, Just adjacent) -> concatMap (go (M.delete Start graph) (Start:path)) adjacent
        (Small _, Just adjacent) -> concatMap (go (M.delete from graph) (from:path)) adjacent
        (_, Just adjacent) -> concatMap (go graph (from:path)) adjacent
