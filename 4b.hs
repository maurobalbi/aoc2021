{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

import AOC

main :: IO ()
main = interactg f

data WinningBoard = WinningBoard [String] [[String]] deriving (Show)

instance Eq WinningBoard
  where
    (==) (WinningBoard _ a) (WinningBoard _ b) = a == b

instance Ord WinningBoard where
  (WinningBoard x _) <= (WinningBoard y _) = length x <= length y

resultFromBoard :: (Num a, Read a) => WinningBoard -> a
resultFromBoard (WinningBoard x y) = read (head x) * sum (map read (join y \\ x))

f :: (Num a, Read a) => [[[Char]]] -> a
f xss = resultFromBoard $ maximum $ getWinningCombination <$> t
  where
    h = splitOn "," $ join $ head xss
    t = map (map words) $ tail xss

    draws = scanl (flip (:)) [] h

    getWinningCombination b = fromMaybe 
      (error "Never wins") 
      $ execState (forM draws (winningCombination b)) Nothing

    winningCombination :: MonadState (Maybe WinningBoard) m => [[String]] -> [String] -> m ()
    winningCombination xss ns = do
      board <- get
      case board of
        Nothing -> if hasBingo xss ns then put $ Just (WinningBoard ns xss) else pure ()
        Just a -> pure ()

hasBingo :: Eq a => [[a]] -> [a] -> Bool
hasBingo xss ns = hasBingo' xss || hasBingo' (transpose xss)
  where
    hasBingo' = any (\xs -> intersect xs ns == xs)
