{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Data.Map as M

main :: IO ()
main = interact  f

f :: [String] -> Int
f xs = (\x -> x !! (length x `div` 2)) $ sort $ calculatePoints <$> brackets
  where
    brackets = mapMaybe (either (const Nothing) Just . parse) xs
    parse = runParser p "" ""
 
calculatePoints :: String -> Int
calculatePoints = foldl' (\acc x -> addPoints x (acc * 5)) 0
  where
    addPoints ')' = (+) 1
    addPoints ']' = (+) 2
    addPoints '}' = (+) 3
    addPoints '>' = (+) 4
    addPoints _ = error "not expected"

p :: Parsec String [Char] [Char]
p =  many1 (parseBrackets ('(',')')
  <|> parseBrackets ('[',']')
  <|> parseBrackets ('<', '>')
  <|> parseBrackets ('{','}')) >> getState

parseBrackets :: (Char, Char) -> Parsec String [Char] ()
parseBrackets b@(o, c) = parseOpeningBracket b >> many p >> (parseClosingBracket c <|> eof) 

parseOpeningBracket :: (Char, Char) -> Parsec String [Char]  ()
parseOpeningBracket (o,c) = do
  char o
  modifyState (c :)
  pure ()

parseClosingBracket :: Char -> Parsec String [Char]  ()
parseClosingBracket c= do
  char c
  modifyState (removeFirst c)
  pure ()

removeFirst :: Char -> String -> String
removeFirst _ [] = []
removeFirst c1 (c2:cs) = if c1 == c2 then cs else c2:removeFirst c1 cs
