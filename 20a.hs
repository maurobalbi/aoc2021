{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import AOC
import Data.Functor
import Data.Tuple
import Debug.Trace
import Data.Either
import qualified Data.Set as S
import qualified Data.Map as M

tupleMap :: [[Bool]] -> M.Map (Int, Int) Bool
tupleMap xss = M.fromList $ do
  (y, row) <- coordinates
  (x, b) <- row
  pure ((x, y), b)
  where
    coordinates = zip [0 ..] $ zip [0 ..] <$> xss

p :: Parser [Bool]
p = many1 (char '.' $> False <|> char '#' $> True)

increaseSpace :: Bool -> M.Map (Int,Int) Bool-> M.Map (Int,Int) Bool
increaseSpace b m = M.union m m'
  where
    m' = M.fromList $ concatMap neighbours' $ M.toAscList m
    neighbours' ((x,y), _) = [((x + x',y + y'), b) | x' <- [-1..1] , y' <- [-1..1]]

applyAlg :: Bool -> [Bool] -> (M.Map (Int,Int) Bool, Bool) -> (M.Map (Int,Int) Bool, Bool)
applyAlg flip ls (m, b) = (M.mapWithKey apply' $ increaseSpace background m, background)
  where
    apply' k a = ls !! f k
    background = flip && not b
    f c = fromMaybe (error "not expected") $ readBin $ binary c m
    binary (xa,ya) m = [fromMaybe background $ M.lookup (xa + x,ya + y) m | y <- [-1..1], x <- [-1..1] ]

run x = length $ M.filter id $ (\x -> fst $ x !! 2) $ iterate (applyAlg (head enhancement) enhancement) (picture, head enhancement)
  where
    enhancement = fromRight [] $ parse p $ (!! 0) . head $ x
    picture = tupleMap $ parseList p <$> ( !! 0) . tail $ x

main :: IO ()
main = interactg run

