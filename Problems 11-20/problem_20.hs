removeAt :: Int -> [a] -> (a, [a])
removeAt n ls = (last l, take m l ++ r)
  where
    (l, r) = splitAt n ls
    m = length l - 1
