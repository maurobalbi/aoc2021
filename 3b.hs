{-# LANGUAGE NoImplicitPrelude #-}

import AOC
import Debug.Trace
import Data.ByteString.Builder (integerDec)
import GHC.Hs.Decls (HsGroup(hs_derivds))

main :: IO ()
main = interact $ f . map (map (== '1'))

f xss = fromMaybe 0 $ readBin co2 * readBin o2 
  where
    co2 = go (>=) [] xss
    o2 = go (<) [] xss
    
    go c h [xs] = h ++ xs
    go c h xss = go c (h ++ [r]) xss'
      where 
        r = count True hs `c` count False hs
        hs = map head xss
        xss' = map tail $ filter ((== r) . head ) xss

