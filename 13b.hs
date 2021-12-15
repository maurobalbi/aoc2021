{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Data.MultiSet as M
import Debug.Trace
import System.IO.Unsafe

main :: IO ()
main = interactg run

type Mark = (Int, Int)

data Fold = X Int | Y Int deriving (Show)

p :: Parser Mark
p = do
  x <- integer
  char ','
  y <- integer
  pure (x, y)

parseFold :: Parser Fold
parseFold = do
  string "fold along "
  parseDirection
  where
    parseDirection =
      X <$> (char 'x' *> char '=' *> integer)
        <|> (Y <$> (char 'y' *> char '=' *> integer))

printMarksLine :: Int -> [(Int,Int)] -> String
printMarksLine y xs = map (\x -> if (x,y) `elem` xs then '#' else ' ') [0..maxX]
  where
   maxX = maximum $ fst <$> xs
   maxY = maximum $ snd <$> xs

printMarks xs = "\n" ++ unlines (map (`printMarksLine` xs) [0..maxY])
  where
   maxY = maximum $ snd <$> xs

run x = unsafePerformIO (putStrLn solve >> pure "")
  where
    solve = printMarks $ M.distinctElems $ foldl (\acc x -> foldOn x `M.map` acc) (M.fromList marks) folds
    folds = parseList parseFold $ (!! 1) x
    marks = parseList p $ head x

foldOn :: Fold -> Mark -> Mark
foldOn (X fx) m@(x, y)
  | x <= fx = m
  | x > fx = (x - 2 * (x - fx), y)
  | otherwise = error "what"
foldOn (Y fy) m@(x,y)
  | y <= fy = m
  | y > fy = (x, y -2 * (y - fy))
  | otherwise = error "what"

