{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.Either
import Debug.Trace

p :: Parser (Int,Int)
p = do
  string "Player 1 starting position: "
  p1 <- integer 
  space
  string "Player 2 starting position: "
  p2 <- integer 
  pure (p1, p2)

run x = step (cycle [1..100]) (0,0) x 0
  where step die s@(score1, score2) l@(loc1,loc2) count 
          | score1 >= 1000 =  score2 * count
          | score2 >= 1000 =  score1 * count
          | count >= 10000 = error "bla"
          | odd count = step (drop 3 die) (score1, score2 + newLoc loc2) (loc1, newLoc loc2) (count + 3)
          | otherwise = step (drop 3 die) (score1 + newLoc loc1, score2) (newLoc loc1, loc2) (count + 3)
            where 
              newLoc loc = (loc + throw) `mod` 10
              throw =  sum $ take 3 die

main :: IO ()
main = interact' $ either (error . show) run . parse p

