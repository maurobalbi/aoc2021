{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

import AOC
import qualified Data.Map as M

main :: IO ()
main = interact $ run . map (read @Int)

eval xs = evalState (f xs) 0

run :: [Int] -> Int
run xs = execState (f xs) 0

f :: MonadState Int m => [Int] -> m [Int]
f xs = do
  if 9 `elem` xs -- have to iterate over all 9s e.g. forM
    then f ((+ 1) <$> xs)
    else do 
      modify (+ updates)
      pure $ reset <$> xs
  where
    flash = filter (> 8) xs
    updates = length flash
    reset x
      | x > 8 = 0
      | otherwise = x
