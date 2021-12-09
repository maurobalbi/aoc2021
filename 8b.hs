{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

import AOC

main :: IO ()
main = interact f

filterLength :: Int -> [String] -> [String]
filterLength l = filter ((l ==) . length)

parseDigits :: String -> [String]
parseDigits xs = words $ last $ splitOn "|" xs

parseNumbers :: String -> [String]
parseNumbers xs = words $ head $ splitOn "|" xs

f :: [String] -> Int
f = sum . map (read . f')

f' :: String -> String
f' xs = character h <$> t
  where
    h = parseNumbers xs
    t = parseDigits xs

character :: [String] -> String -> Char
character xss i
  | length i == 2 = '1'
  | length i == 3 = '7'
  | length i == 4 = '4'
  | length i == 7 = '8'
  | null (getSegments [1, 2, 3, 5, 6, 7] segment \\ i) && length i == 6 = '0'
  | null (getSegments [1, 3, 4, 5, 7] segment \\ i) && length i == 5 = '2'
  | null (getSegments [1, 3, 4, 6, 7] segment \\ i) && length i == 5 = '3'
  | null (getSegments [1, 2, 4, 6, 7] segment \\ i) && length i == 5 = '5'
  | null (getSegments [1, 2, 4, 5, 6, 7] segment \\ i) && length i == 6 = '6'
  | null (getSegments [1, 2, 3, 4, 6, 7] segment \\ i) && length i == 6 = '9'
  | otherwise = error "No letter found"
  where
    segment = findPermutation xss

getSegments :: [Int] -> String -> String
getSegments i xs = foldr (\x acc -> xs !! (x - 1) : acc) "" i

findPermutation :: [String] -> String
findPermutation xss = do
  [one, two, three, four, five, six, seven] <- permutations "abcdefg"

  guard $ constraint 3 one 6
  guard $ constraint 3 one 5
  guard $ constraint 1 one 3
  guard $ constraint 3 two 6
  guard $ constraint 1 two 5
  guard $ constraint 1 two 4
  guard $ constraint 2 three 6
  guard $ constraint 2 three 5
  guard $ constraint 1 three 4
  guard $ constraint 1 three 3
  guard $ constraint 1 three 2
  guard $ constraint 2 four 6
  guard $ constraint 3 four 5
  guard $ constraint 1 four 4
  guard $ constraint 2 five 6
  guard $ constraint 1 five 5
  guard $ constraint 3 six 6
  guard $ constraint 2 six 5
  guard $ constraint 1 six 4
  guard $ constraint 1 six 3
  guard $ constraint 1 six 2
  guard $ constraint 3 seven 6
  guard $ constraint 3 seven 5

  [one, two, three, four, five, six, seven]
  where
    constraint i c n = i == length (filter (\x -> c `elem` x) (filterLength n))
    filterLength n = filter ((n ==) . length) xss

-- # \ seg: 1 2 3 4 5 6 7   length
-- 0        1 1 1 0 1 1 1   6
-- 1        0 0 1 0 0 1 0   2
-- 2        1 0 1 1 1 0 1   5
-- 3        1 0 1 1 0 1 1   5
-- 4        0 1 1 1 0 1 0   4
-- 5        1 1 0 1 0 1 1   5
-- 6        1 1 0 1 1 1 1   6
-- 7        1 0 1 0 0 1 0   3
-- 8        1 1 1 1 1 1 1   7
-- 9        1 1 1 1 0 1 1   6

-- segment 1 in all with length 6 in all with length 5                      in all with length 3
-- segment 2 in all with length 6 in   1 with length 5 in all with length 4
-- segment 3 in   2 with length 6 in   2 with length 5 in all with length 4 in all with length 3 in all with length 2
-- segment 4 in   2 with length 6 in all with length 5 in all with length 4
-- segment 5 in   2 with length 6 in   1 with length 5
-- segment 6 in all with length 6 in   2 with length 5 in all with length 4 in all with length 3 in all with length 2
-- segment 7 in all with length 6 in all with length 5