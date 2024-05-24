dropEvery :: [a] -> Int -> [a]
dropEvery l n = map snd . filter ((0 /=) . (`mod` n) . fst) $ zip [1 ..] l
