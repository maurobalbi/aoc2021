{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import AOC hiding (transpose)
import qualified Data.Set as S
import qualified Data.Map as M
import RIO.List (headMaybe)
import Control.Lens
import Linear
import Data.Either (partitionEithers)

type Point = V3 Int

type Scanner = [Point]

p :: Parser (V3 Int)
p = V3 <$> signedInt <* char ',' <*> signedInt <* char ',' <*> signedInt
  where
    signedInt =
      read
        <$> ( do
                i <- char '-' <|> digit
                rest <- many digit
                pure $ i : rest
            )
parseBeacons :: Functor f => f [String] -> f Scanner
parseBeacons x = parseList p <$> beacons
  where
    beacons = tail <$> x

align :: [(Scanner, Point)] -> [Scanner] -> [Scanner] -> [(Scanner, Point)]
align result _ [] = result
align result (ref:refs) scanners = align (found ++ result) (map fst found ++ refs) notFound
  where
    (found, notFound) =
      partitionEithers
        [ maybe (Right scanner) Left . headMaybe $ align' ref scanner
        | scanner <- scanners
        ]

align' :: Scanner -> Scanner -> [(Scanner, Point)]
align' a b = [( (+) pos <$> o, pos) | o <- orientations b, pos <- overlap a o]

overlap :: Scanner -> Scanner -> [Point]
overlap a b = M.keys . M.filter (>=12) . M.fromListWith (+) . map (, 1) $ (-) <$> a <*> b

orientations :: Scanner -> [Scanner]
orientations ps =
  [ (\pos -> V3 (a * x pos) (b * y pos) (c* z pos)) <$> ps
  | [x,y,z] <- permutations [(^. _x),(^. _y),(^. _z)]
  , [a,b,c] <- mapM (const [-1,1]) ":&)"
  ]
pick2 :: [a] -> [(a,a)]
pick2 [] = []
pick2 (x:xs) = map (x ,) xs ++ pick2 xs

manhattan :: Point -> Point -> Int
manhattan a b = sum $ abs $  a - b

run :: [Scanner] -> Int
run (x:xs) = maximum . map (uncurry manhattan) . pick2 . map snd $ aligned
  where
    aligned = align [(x, V3 0 0 0)] [x] xs

main :: IO ()
main = interactg $ run . parseBeacons

