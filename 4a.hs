{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Data.ByteString.Builder (integerDec)

main :: IO ()
main = interactg f

f xss = t
  where
    h = head xss
    t = tail xss
