{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import qualified Control.Applicative as C

main :: IO ()
main = interact $ run . parseList p

data Snail a = Number a | Pair (Snail a) (Snail a) deriving (Foldable, Eq)

instance Show a => Show (Snail a) where
  show (Number a) = show a
  show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"

p :: Parser (Snail Int)
p = do
  char '['
  p1 <- Number <$> integer <|> p
  char ','
  p2 <- Number <$> integer <|> p
  char ']'
  pure $ Pair p1 p2

reduce :: Snail Int -> Snail Int
reduce = fromJust . reduce'
  where
    reduce' s = (explode s >>= reduce') C.<|> (splitPair s >>= reduce') C.<|> pure s

explode :: Snail Int -> Maybe (Snail Int)
explode s = snd <$> explode' 0 s
  where
    explode' 4 (Pair (Number l) (Number r)) = Just ((l, r), Number 0)
    explode' _ (Number _) = Nothing
    explode' i (Pair l r) =
      (\((nl, nr), p) -> ((nl, 0), Pair p (left nr r))) <$> explode' (i + 1) l
        C.<|> (\((nl, nr), p) -> ((0, nr), Pair (right nl l) p)) <$> explode' (i + 1) r

splitPair :: Snail Int -> Maybe (Snail Int)
splitPair (Number n)
  | n >= 10 = Just $ Pair (Number $ n `div` 2) (Number $ succ n `div` 2)
  | otherwise = Nothing
splitPair (Pair l r) = flip Pair r <$> splitPair l C.<|> Pair l <$> splitPair r

magnitude :: Snail Int -> Int
magnitude (Number n) = n
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

left x (Number y) = Number (x + y)
left x (Pair l r) = Pair (left x l) r

right x (Number y) = Number (x + y)
right x (Pair l r) = Pair l (right x r)

run :: [Snail Int] -> Int
run input = maximum [magnitude $ reduce (Pair a b) | a <- input, b <- input, a /= b]