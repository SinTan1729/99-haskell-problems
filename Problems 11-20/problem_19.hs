rotate :: [a] -> Int -> [a]
rotate l n =
  if n >= 0
    then (\(x, y) -> y ++ x) $ splitAt n l
    else rotate l (length l + n)
