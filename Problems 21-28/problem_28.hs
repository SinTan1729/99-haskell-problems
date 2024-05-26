import Data.List

lsort :: [[a]] -> [[a]]
lsort = sortBy (\x y -> length x `compare` length y)

lfsort :: [[a]] -> [[a]]
lfsort ls = sortBy (\x y -> count x `compare` count y) ls
  where
    count x = length . filter (length x ==) $ map length ls
