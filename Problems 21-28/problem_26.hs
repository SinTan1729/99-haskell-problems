combinations :: Int -> [a] -> [[a]]
combinations 0 _ = []
combinations _ [] = []
combinations n (hd : tl) = with ++ without
  where
    with =
      if n == 1
        then [[hd]]
        else [(hd :)] <*> combinations (n - 1) tl
    without = combinations n tl
