{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Data.Map as M
import qualified Data.MultiSet as S
import Debug.Trace
import Text.Parsec.Char (upper)

main :: IO ()
main = interactg run

type Polymer = (String, Char)

p :: Parser Polymer
p = do
  x <- many1 upper
  string " -> "
  y <- upper
  pure (x, y)

insertElements :: M.Map String Char -> (Char, Char) -> [(Char,Char)]
insertElements m (a,b) = [(a,c), (c,b)]
  where c = fromMaybe (error "Should not happen") $ M.lookup (a:b:"") m

linkChain :: M.Map String Char -> S.MultiSet (Char,Char) -> S.MultiSet (Char,Char)
linkChain m = S.concatMap (insertElements m)

run x = last (occurences fold) - head (occurences fold)
  where
    fold = S.foldOccur (\ (a,b) i acc -> S.insertMany a i $ S.insertMany b i $ acc) S.empty iteration
    occurences x = sort $ (`div` 2 ) . (+1) .snd <$> S.toOccurList x
    iteration = (!! 40) $ iterate (linkChain polymers) pairs
    pairs = S.fromList $ zip chain $ drop 1 chain
    chain = (!! 0 ) $ parseList (many1 upper) $ head x
    polymers = M.fromList $ parseList p $ (!! 1) x


