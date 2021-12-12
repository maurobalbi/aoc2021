{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

import AOC
import qualified Data.Map.Strict as M
import Debug.Trace

main :: IO ()
main = interact $ run . map (map (read @Int . (: "")) :: String -> [Int])

data Octopus = Exhausted | Energy Int deriving (Eq, Show, Ord)

type Flashes = Int
type Point = (Int,Int)

inc :: Octopus -> Octopus
inc (Energy n) = Energy $ n + 1
inc Exhausted   = Exhausted

run :: [[Int]] -> Int
run xs = (+1) . fromJust . elemIndex 100 . evalState (replicateM 400 step) $ tupleMap xs

unflash :: Octopus -> Octopus
unflash (Energy n) = Energy n
unflash Exhausted = Energy 0

updatePoint :: (Ord a1, Ord b) => (a2 -> a2) -> (a1, b) -> M.Map (a1, b) a2 -> M.Map (a1, b) a2
updatePoint f p@(x,y)= M.adjust f p

updatePoints :: (a->a) -> [Point] -> M.Map Point a -> M.Map Point a
updatePoints f ps m = foldl (flip $ updatePoint f) m ps 

step :: MonadState (M.Map Point Octopus) m => m Flashes
step = do
  modify $ updateAll inc
  nFlashes <- flash
  modify $ updateAll unflash
  pure nFlashes

neighbours :: Point -> [Point]
neighbours (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

flash :: MonadState (M.Map (Int,Int) Octopus) m => m Flashes
flash = do
  flashing <- gets $ M.filter (>= Energy 10)
  let nFlashing = length flashing
  if nFlashing == 0 then pure 0 else do 
    modify $ updatePoints (const Exhausted) $ M.keys flashing
    let neighbouringPoints = concatMap neighbours $ M.keys flashing
    modify $ updatePoints inc neighbouringPoints
    nFlashing' <- flash
    pure $ nFlashing + nFlashing'

updateAll = M.map

tupleMap :: [[Int]] -> M.Map (Int, Int) Octopus
tupleMap xss = M.fromList $ do
  (y, row) <- coordinates
  (x, nr) <- row
  pure ((x, y), Energy nr)
  where
    coordinates = zip [0 ..] $ zip [0 ..] <$> xss
