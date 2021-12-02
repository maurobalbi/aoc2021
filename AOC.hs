{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables        #-}


module AOC(module Prelude, module AOC, module Control.Monad.State, module Text.Parsec, module Data.List, module Data.List.Split) where

import Prelude hiding (interact)
import qualified Prelude
import Control.Monad.State
import Data.List
import Data.List.Split hiding (endBy, sepBy, oneOf)
import Text.Parsec hiding (count, parse, uncons, State)
import qualified Text.Parsec as Parsec
import Data.Char

interact :: Show a => ([String] -> a) -> IO ()
interact f = interact' $ f . lines

interact' :: Show a => (String -> a) -> IO ()
interact' f = Prelude.interact $ (++ "\n") . show . f

count :: Eq a => a -> [a] -> Int 
count c = length . filter (==c)

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
enump = choice $ map sr [minBound :: b .. maxBound ::b]
  where
    sr :: (Show b) => b -> Parser b
    sr x = try $ stringi (show x) >> return x
