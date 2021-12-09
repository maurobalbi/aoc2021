{-# LANGUAGE NoImplicitPrelude #-}

import AOC hiding ((!!))
import Data.List.Safe ((!!))

main :: IO ()
main = interact $ f . map (map (read . (: "")) :: String -> [Int])

f :: [[Int]] -> Int
f xss = sum . map (+ 1) . f'' . f' . zip [0 ..] $ zip [0 ..] <$> xss

f' :: Monad m => m (b1, m (a, b2)) -> m ((a, b1), b2)
f' xss = do
  (y, row) <- xss
  (x, nr) <- row
  pure ((x, y), nr)

f'' :: [((Int, Int), Int)] -> [Int]
f'' xss = do
  ((x, y), nr) <- xss
  guard $ nr < fromMaybe maxBound (lookup (x -1, y) xss)
  guard $ nr < fromMaybe maxBound (lookup (x + 1, y) xss)
  guard $ nr < fromMaybe maxBound (lookup (x, y -1) xss)
  guard $ nr < fromMaybe maxBound (lookup (x, y + 1) xss)
  pure nr
