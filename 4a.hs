{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Debug.Trace

main :: IO ()
main = interactg f

f :: [[[Char]]] -> Int
f xss = read (fst goH) * sum (map read (snd goH))
  where
    h = splitOn "," $ join $ head xss
    t = map (map (filter (/= "") . splitOn " ")) $ tail xss

    goH = head go

    go = do
      i <- scanl (flip (:)) []  h
      ts <- filter (hasBingo i) t
      pure (head i, join ts \\ i)

hasBingo :: Eq a => [a] -> [[a]] -> Bool
hasBingo ns xss = hasBingo' xss || hasBingo' (transpose xss)
  where
    hasBingo' = any (\xs -> intersect xs ns == xs)
