{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFoldable #-}

import AOC hiding (count, Parser, parse)

import Debug.Trace
import Numeric
import Data.Either
import Data.Functor
import Data.Functor.Identity
import qualified Prelude as P
import Data.Char
import Text.Parsec (count, parse)

data Packet a = Literal {version:: a, value::Int}
  | Operator {version::a, oType::Int, packets::[Packet a]}
  deriving (Show, Foldable)

type Parser a = Parsec String Int a

main :: IO ()
main = P.interact run

parseHex :: Parsec String () String
parseHex =  do
  x <- many1 hexToString
  pure $ join x

hexToString :: Parsec String () String
hexToString = do
  c <- hexDigit
  pure $ case toUpper c of
    '0' -> "0000"
    '1' -> "0001"
    '2' -> "0010"
    '3' -> "0011"
    '4' -> "0100"
    '5' -> "0101"
    '6' -> "0110"
    '7' -> "0111"
    '8' -> "1000"
    '9' -> "1001"
    'A' -> "1010"
    'B' -> "1011"
    'C' -> "1100"
    'D' -> "1101"
    'E' -> "1110"
    'F' -> "1111"
    _ -> error "Not a hex number!" ++ show c

parsePacket :: Parser (Packet Int)
parsePacket = do
  try parseLiteral <|> parseOperator

parseOperator :: Parser (Packet Int)
parseOperator = do
  version <- readBin' <$> count 3 digit
  pType <- count 3 digit
  x <- digit
  packets <- case x of
    '0' -> parseByLengthPacket
    '1' -> parseByNumberPacket
    _   -> error "Unexpected"
  pure $ Operator version (readBin' pType) packets

parseByLengthPacket :: Parser [Packet Int]
parseByLengthPacket = do
  length <- readBin' <$> count 15 digit
  bits <- count length digit
  let packets = fromRight [] $ runParser (many parsePacket) 0 "" bits
  pure packets

parseByNumberPacket :: Parser [Packet Int]
parseByNumberPacket = do
  length <- readBin' <$> count 11 digit
  count length parsePacket

readBin' :: [Char] -> Int
readBin' = fromMaybe (error "Not a binary") . readBin

parseLiteral :: Parser (Packet Int)
parseLiteral = do 
  version <- readBin' <$> count 3 digit
  string "100"
  Literal version <$> parseBits

parseBits :: Parser Int
parseBits = do
  packet <- manyTill (char '1' *> count 4 digit) (try $ lookAhead (char '0' *> count 4 digit))
  finalPacket <- char '0' *> count 4 digit
  pure $ readBin' $ concat $ packet ++ [finalPacket]

run x = either show (show . foldl (+) 0) $ do
  input <- trace (show parseH) parseH
  runParser parsePacket 0 "" input
    where
      parseH = parse parseHex "" x