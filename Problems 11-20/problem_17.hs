split :: [a] -> Int -> ([a], [a])
split x 0 = ([], x)
split (hd : tl) n = (hd : next_hd, next_tl)
  where
    (next_hd, next_tl) = split tl (n - 1)
