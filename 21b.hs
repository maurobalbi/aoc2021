{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.Bifunctor
import Data.Either
import qualified Data.Map as M
import Data.Tuple
import Debug.Trace

p :: Parser (Int, Int)
p = do
  string "Player 1 starting position: "
  p1 <- integer
  space
  string "Player 2 starting position: "
  p2 <- integer
  pure (p1, p2)

type GameState = M.Map (Int, Int) (Int, Int)

move n pos = ((pos + n -1) `rem` 10) + 1

run (loc1, loc2) = let (x, y) = evalState (play ((loc1, 0), (loc2, 0))) M.empty in max x y

type Game = ((Int, Int), (Int, Int))

play :: Game -> State (M.Map Game (Int, Int)) (Int, Int)
play game@((loc1, score1), (loc2, score2))
  | score1 >= 21 = pure (1, 0)
  | score2 >= 21 = pure (0, 1)
  | otherwise = do
    dp <- get
    case dp M.!? game of
      Just res -> return res
      Nothing -> do
        res <-
          swap . foldl (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) (0, 0)
            <$> mapM
              play
              [ ((loc2, score2), (loc1', score1 + loc1'))
                | d <- sum <$> replicateM 3 [1, 2, 3],
                  let loc1' = move d loc1
              ]
        modify (M.insert game res)
        return res

lookupCoefficient :: M.Map Int Int
lookupCoefficient = M.fromList [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

main :: IO ()
main = interact' $ either (error . show) run . parse p
