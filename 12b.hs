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
import qualified Data.Set as S

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
routeS g = go g [] False Start
  where 
    go graph path visited from = 
      case (from, M.lookup from graph) of
        (End, _) -> [End: path]
        (_, Nothing) -> []
        (Start, Just adjacent) -> concatMap (go (M.delete Start graph) (Start:path) False) adjacent
        (Small _, Just adjacent) -> 
          if visited
            then concatMap (go (M.delete from graph) (from:path) True) adjacent
            else if isJust $ find (==from) path
              then concatMap (go (trace ((show $ filterMap graph) ++ (show $ filterMap2 graph (filter isSmall path))) filterMap2 graph (filter isSmall path)) (from:path) True) adjacent
            else
              concatMap (go graph (from:path) False) adjacent
        (_, Just adjacent) -> concatMap (go graph (from:path) visited) adjacent
    filterMap = M.filterWithKey (\x _ -> isSmall x)
    filterMap2 graph = foldl (flip M.delete) graph 

isSmall :: Node -> Bool
isSmall (Small _) = True 
isSmall _ = False