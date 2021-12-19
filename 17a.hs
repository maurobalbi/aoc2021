{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFoldable #-}

import AOC
import Debug.Trace
import Data.Either
import qualified Prelude as P

-- x > 0 && x < (max targetAreaX)
-- y > (min targetAreaY) && (y - x * (x+1) /2) < max targetArea

-- max amount steps = max targetAreaX

main :: IO ()

main = interact' $ run . fromRight (error "Parse error") . parse p

p :: Parser ((Int,Int), (Int,Int))
p = do
  string "target area: x="
  xMin <- integer
  string ".."
  xMax <- integer
  string ", y="
  yMin <- read <$> (do
    i <- char '-' <|> digit
    rest <- many digit 
    pure $ i: rest)
  string ".."
  yMax <- read <$> (do
    i <- char '-' <|> digit
    rest <- many digit 
    pure $ i: rest)
  pure ((xMin,xMax), (yMin, yMax))

step :: ((Int,Int),(Int,Int)) -> ((Int,Int), (Int,Int))
step ((x,y), (vx,vy)) = ((x+vx, y+vy), (decrease vx, vy - 1)) 
  where
    decrease vx = if vx == 0 then 0 else vx -1

isInTarget :: ((Int,Int), (Int,Int)) -> ((Int,Int), (Int,Int)) -> Bool 
isInTarget ((xMin, xMax), (yMin, yMax)) ((x,y), _) = x >= xMin && x <= xMax && y >= yMin && y <= yMax

hits m@((xMin,xMax), (yMin,yMax)) = do
  (vx, vy) <- [(vx,vy) | vx <- [0..xMax], vy <- [yMin.. (vx * (vx +1) `div` 2)]]
  let steps = takeWhile (\((x,y), _) -> x <= xMax && y >= yMin - abs vy) $ iterate step ((0,0), (vx, vy))
  guard $ any (isInTarget m) steps
  -- guard $ isInTarget m ((x,y), (vx', vy'))
  snd . fst <$> steps

run x = maximum $ hits x