range :: Int -> Int -> [Int]
range m n = if m > n then [] else m : range (m + 1) n
