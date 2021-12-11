{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Data.Map as M
import Text.Parsec.Error (errorMessages, messageString)

main :: IO ()
main = interact $ f . parseParens

p = do
  char '(' >> many p >> char ')'
  <|> (char '[' >> many p >> char ']')
  <|> (char '<' >> many p >> char '>')
  <|> (char '{' >> many p >> char '}')

parseParens :: [String] -> Int
parseParens = sum . map (either handleParseError id . parse (p >> pure 0))

handleParseError :: ParseError -> Int 
handleParseError pe = case messageString $ head $ errorMessages pe of
  "\")\"" -> 3
  "\"]\"" -> 57
  "\"}\"" -> 1197
  "\">\"" -> 25137
  _ -> 0
  

f = id

