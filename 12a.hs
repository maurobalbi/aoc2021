{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

import AOC
import qualified Data.Map as M
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
    start = Start <$ string "start"
    end = End <$ string "end"
    big = Big <$> many1 upper
    small = Small <$> many1 lower

buildMap :: [(Node,Node)] -> M.Map Node [Node]
buildMap xs = M.fromListWith (++) $ second pure <$> (xs ++ (swap <$> xs))

run = buildMap

neighbours :: Ord k => M.Map k [a] -> k -> [a]
neighbours g n = fromMaybe [] $ M.lookup n g

routeS :: Graph -> Node -> Node -> State [Node] Bool
routeS g dest from | dest == from = return True
routeS g dest from = do
      seen <- get
      if from `elem` seen then return False else do
      put (from:seen)
      anyM (routeS g dest) (neighbours g from)

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM p [] = return False
anyM p (x:xs) = do
    y <- p x
    if y
      then return True
      else anyM p xs