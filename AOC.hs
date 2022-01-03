{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module AOC (module Prelude, module AOC, module Control.Monad.State, module Data.Vector, module Text.Parsec, module Data.List, module Data.List.Split, module Data.Maybe) where

import Control.Monad.State
import Data.Char
import Data.List
import Data.List.Split hiding (endBy, oneOf, sepBy)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V
import Text.Parsec hiding (State, count, parse, uncons, Line)
import qualified Text.Parsec as Parsec
import Prelude hiding (interact)
import qualified Prelude

interact :: Show a => ([String] -> a) -> IO ()
interact f = interact' $ f . lines

interact' :: Show a => (String -> a) -> IO ()
interact' f = Prelude.interact $ (++ "\n") . show . f

interactg :: Show a => ([[String]] -> a) -> IO ()
interactg f = interact $ f . splitOn [""]

count :: Eq a => a -> [a] -> Int
count c = length . filter (== c)

type Parser = Parsec String ()

parse :: Parser a -> String -> Either ParseError a
parse p = Parsec.parse p ""

parseList :: Parser a -> [String] -> [a]
parseList p = either (error . show) id . mapM (parse p)

chari :: Char -> Parser Char
chari c = oneOf [toLower c, toUpper c]

stringi :: String -> Parser String
stringi = mapM chari

integer :: Parser Int
integer = read <$> many1 digit

signedInteger :: Parser Int
signedInteger = read
        <$> ( do
                i <- char '-' <|> digit
                rest <- many digit
                pure $ i : rest
            )

enump :: forall b. (Enum b, Bounded b, Show b) => Parser b
enump = choice $ map sr [minBound :: b .. maxBound :: b]
  where
    sr :: (Show b) => b -> Parser b
    sr x = try $ stringi (show x) >> return x

readBin :: ReadBin a => [a] -> Maybe Int
readBin = foldl' add (Just 0)
  where
    add x y = do
      x' <- x
      y' <- toBin y
      return $ x' * 2 + y'

class ReadBin a where
  toBin :: a -> Maybe Int

instance ReadBin Char where
  toBin '0' = Just 0
  toBin '1' = Just 1
  toBin _ = Nothing

instance ReadBin Bool where
  toBin False = Just 0
  toBin True = Just 1

instance Num a => Num (Maybe a) where
  x * y       = (*) <$> x <*> y
  x + y       = (+) <$> x <*> y
  abs         = (abs <$>)
  signum      = (signum <$>)
  fromInteger = (Just . fromInteger)
  negate      = (negate <$>)

(!|) :: Vector a -> Int -> a
v !| i = v V.! (i `mod` V.length v)

-- | The 'ltov' function is a convenience function for 'Vector.fromList'.
ltov :: [a] -> Vector a
ltov = V.fromList

-- | The 'ltov2' function converts a list of lists into a 'Vector' of 'Vector's.
ltov2 :: [[a]] -> Vector (Vector a)
ltov2 = ltov . map ltov

-- | The 'ltov3' function converts a list of lists of lists into a 'Vector' of 'Vector's of 'Vector's.
ltov3 :: [[[a]]] -> Vector (Vector (Vector a))
ltov3 = ltov . map ltov2

-- | The 'ltov4' function converts a list of lists of lists of lists into a 'Vector' of 'Vector's of 'Vector's of 'Vector's.
ltov4 :: [[[[a]]]] -> Vector (Vector (Vector (Vector a)))
ltov4 = ltov . map ltov3

-- | The 'vtol' function is a convenience function for 'Vector.toList'.
vtol :: Vector a -> [a]
vtol = V.toList

-- | The 'vtol2' function converts a 'Vector' of 'Vector's into a list of lists.
vtol2 :: Vector (Vector a) -> [[a]]
vtol2 = map vtol . vtol

-- | The 'vtol2' function converts a 'Vector' of 'Vector's of 'Vector's into a list of lists of lists.
vtol3 :: Vector (Vector (Vector a)) -> [[[a]]]
vtol3 = map vtol2 . vtol

-- | The 'vtol2' function converts a 'Vector' of 'Vector's of 'Vector's of 'Vector's into a list of lists of lists of lists.
vtol4 :: Vector (Vector (Vector (Vector a))) -> [[[[a]]]]
vtol4 = map vtol3 . vtol

instance (Num a, Num b, Num c) => Num (a, b, c) where
  (x, y, z) + (u, v, w) = (x + u, y + v, z + w)
  (x, y, z) * (u, v, w) = (x * u, y * v, z * w)
  negate (x, y, z) = (negate x, negate y, negate z)
  fromInteger x = (fromInteger x, 0, 0)
  abs (x, y, z) = (abs x, abs y, abs z)
  signum (x, y, z) = (signum x, signum y, signum z)