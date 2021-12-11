{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Data.Map as M

main :: IO ()
main = interact $ f . map (map (read . (: "")) :: String -> [Int])

f :: [[Int]] -> Int
f xss = sum . map (+ 1) . f'' . M.fromList . f' . zip [0 ..] $ zip [0 ..] <$> xss

f' :: Monad m => m (b1, m (a, b2)) -> m ((a, b1), b2)
f' xss = do
  (y, row) <- xss
  (x, nr) <- row
  pure ((x, y), nr)

f'' :: M.Map (Int, Int) Int -> [Int]
f'' xss = do
  ((x, y), nr) <- M.toList xss
  guard $ nr < fromMaybe maxBound (M.lookup (x -1, y) xss)
  guard $ nr < fromMaybe maxBound (M.lookup (x + 1, y) xss)
  guard $ nr < fromMaybe maxBound (M.lookup (x, y -1) xss)
  guard $ nr < fromMaybe maxBound (M.lookup (x, y + 1) xss)
  pure nr
