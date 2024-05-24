slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = error "Invalid indices"
slice l 1 n = take n l
slice (_ : tl) m n = slice tl (m - 1) (n - 1)
