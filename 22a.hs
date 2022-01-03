{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.Either
import qualified Data.Map as M
import Debug.Trace

type Limit = ((Int,Int),(Int,Int),(Int,Int))

data Switch = On Limit | Off Limit deriving (Show, Eq, Ord)

type Point = (Int,Int,Int)

type Grid = M.Map Point Bool

switch :: Parser Switch
switch = do
  try on <|> off
    where on = string "on " >> On <$> points
          off = string "off " >> Off <$> points

points :: Parser Limit
points = do
  string "x="
  xMin <- signedInteger
  string ".."
  xMax <- signedInteger
  string ",y="
  yMin<- signedInteger 
  string ".."
  yMax <- signedInteger
  string ",z="
  zMin<-signedInteger 
  string ".."
  zMax <- signedInteger
  pure ((xMin,xMax),(yMin, yMax),(zMin,zMax))

step :: Switch -> Grid -> Grid
step (On l) g = foldl' (\acc p -> M.insert p True acc) g $ expand l
step (Off l) g = foldl' (\acc p -> M.insert p False acc) g $ expand l

expand :: Limit -> [Point]
expand ((xMin,xMax),(yMin,yMax),(zMin,zMax)) = do
  x <- [xMin..xMax]
  y <- [yMin..yMax]
  z <- [zMin..zMax]
  pure (x,y,z)

run x = M.size $ M.filter (==True) $ foldl (flip step) M.empty part1
  where part1 = filter inLimit x

inLimit (Off p) = inLimit $ On p
inLimit (On (x,y,z)) =  inLimit' x && inLimit' y && inLimit' z
  where inLimit' (a,b) = a>= -50 && b <= 50

main :: IO ()
main = interact $ run . parseList switch

