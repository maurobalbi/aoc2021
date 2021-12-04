{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.ByteString.Builder (integerDec)

main :: IO ()
main = interact $ f . map (map (== '1'))

f :: [[Bool]] -> Int
f xss = fromMaybe 0 $ readBin r * readBin (map not r)
  where
    r = map c $ transpose xss
    c xs = count True xs > count False xs