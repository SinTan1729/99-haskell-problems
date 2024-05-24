repli :: [a] -> Int -> [a]
repli x n = concat $ [replicate n] <*> x
