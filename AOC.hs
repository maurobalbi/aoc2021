{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module AOC (module Prelude, module AOC, module Control.Monad.State, module Text.Parsec, module Data.List, module Data.List.Split, module Data.Maybe) where

import Control.Monad.State
import Data.Char
import Data.List
import Data.List.Split hiding (endBy, oneOf, sepBy)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
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