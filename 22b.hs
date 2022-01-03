{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.Either
import qualified Data.MultiSet as M
import Debug.Trace

newtype Span = Span (Int,Int) deriving (Eq, Ord, Show)

mkSpan a b | b < a = error "Inverted Span!"
mkSpan a b = Span (a,b)

spanLength :: Span -> Int
spanLength (Span (a, b)) = b - a + 1

spanIntersection :: Span -> Span -> Maybe Span
spanIntersection (Span (a1,a2)) (Span (b1,b2))
  | max1 <= min2 = Just $ mkSpan max1 min2
  | otherwise = Nothing
  where
    max1 = max a1 b1
    min2 = min a2 b2

data Cube = Cube {
  spanX :: Span,
  spanY :: Span,
  spanZ :: Span
} deriving (Eq, Ord, Show)

cubeVolume :: Cube -> Int
cubeVolume (Cube s1 s2 s3) = spanLength s1 * spanLength s2 * spanLength s3

cubeIntersection :: Cube -> Cube -> Maybe Cube
cubeIntersection (Cube x1 y1 z1) (Cube x2 y2 z2) = do
  xSpan <- spanIntersection x1 x2
  ySpan <- spanIntersection y1 y2
  zSpan <- spanIntersection z1 z2
  pure $ Cube xSpan ySpan zSpan

data CubeState = CubeState {add:: M.MultiSet Cube, subtract :: M.MultiSet Cube}

initialCubeState :: CubeState
initialCubeState = CubeState M.empty M.empty

lightCount :: CubeState -> Int
lightCount (CubeState add substract) = sum (M.map cubeVolume add) - sum (M.map cubeVolume substract)

data Switch = On | Off deriving (Show, Eq, Ord)
data SwitchAction = SwitchAction Switch Cube deriving (Eq, Ord, Show)

applySwitch :: SwitchAction -> CubeState -> CubeState
applySwitch (SwitchAction s c) (CubeState add substract) =
  case s of
    On -> CubeState (M.insert c (M.union subIntersects add)) (M.union addIntersects substract)
    Off -> CubeState (M.union subIntersects add) (M.union addIntersects substract)
  where
    addIntersects = M.mapMaybe (cubeIntersection c) add
    subIntersects = M.mapMaybe (cubeIntersection c) substract

run :: Foldable t => t SwitchAction -> Int
run x = lightCount $ foldl (flip applySwitch) initialCubeState x

main :: IO ()
main = interact $ run . parseList switch

switch :: Parser SwitchAction
switch = do
  try on <|> off
    where on = string "on " >> SwitchAction On <$> points
          off = string "off " >> SwitchAction Off <$> points

points :: Parser Cube
points = do
  string "x="
  xMin <- signedInteger
  string ".."
  xMax <- signedInteger
  string ",y="
  yMin<- signedInteger
  string ".."
  yMax <- signedInteger
  string ",z="
  zMin<-signedInteger
  string ".."
  Cube (mkSpan xMin xMax) (mkSpan yMin yMax) . mkSpan zMin <$> signedInteger