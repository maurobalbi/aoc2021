{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

import AOC
import qualified Data.Map as M
import Debug.Trace

main :: IO ()
main = interact $ run . map (read @Int)

eval xs = evalState (f xs) 0

run :: [Int] -> Int
run xs = execState (f xs) 0

f :: MonadState Int m => [Int] -> m [Int]
f xs = do
  if 9 `elem` xs -- have to iterate over all 9s e.g. forM
    then foldM (\acc x -> f ((\x -> if x == 0 then 0 else x + 1) <$> acc)) xs flash 
    else do 
      modify (+ updates)
      pure $ reset <$> xs
  where
    flash = filter (== 9) xs
    updates = length $ filter (> 8) xs
    reset x
      | x > 8 = 0
      | otherwise = x
