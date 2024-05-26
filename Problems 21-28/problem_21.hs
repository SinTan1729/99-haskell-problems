insertAt :: a -> [a] -> Int -> [a]
insertAt x ls n = left ++ [x] ++ right
  where
    (left, right) = splitAt (n - 1) ls
