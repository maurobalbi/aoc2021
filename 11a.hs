{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

import AOC
import qualified Data.Map.Strict as M
import Debug.Trace

main :: IO ()
main = interact $ run . map (map (read @Int . (: "")) :: String -> [Int])

run :: [[Int]] -> (M.Map (Int,Int) Int, Int)
run xs = runState (stepM 1 $ tupleMap xs) 0

stepM :: MonadState Int m => Int -> M.Map (Int,Int) Int -> m (M.Map (Int,Int) Int)
stepM 1 xs = step xs
stepM i xs = do
  u <- step xs
  stepM (i - 1) u

step :: MonadState Int m => M.Map (Int,Int) Int -> m (M.Map (Int,Int) Int)
step = f . M.map (\x -> if x == 9 then 9 else x + 1)

f :: MonadState Int m => M.Map (Int,Int) Int -> m (M.Map (Int,Int) Int)
f xs = do
  if 9 `elem` xs -- have to iterate over all 9s e.g. forM
    then foldM (\acc !x -> f $ update acc (fst x)) xs $ M.toList flash
    else do
      modify (+ updates)
      pure $ reset <$> xs
  where
    update :: M.Map (Int,Int) Int -> (Int,Int) -> M.Map (Int,Int) Int
    update m (!x,!y) = M.alter (\v -> if v == 0 then 0 else v + 1) (x,y) m
    flash = trace (show $ M.filter (== 9) xs) M.filter (== 9) xs
    updates = length $ M.filter (> 8) xs
    reset x
      | x > 8 = 0
      | otherwise = x

tupleMap :: [[Int]] -> M.Map (Int,Int) Int
tupleMap xss = M.fromList $ do
  (y, row) <- coordinates
  (x, nr) <- row
  pure ((x, y), nr)
    where coordinates = zip [0 ..] $ zip [0 ..] <$> xss