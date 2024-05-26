isqrt :: (Integral n) => n -> n
isqrt = floor . sqrt . fromIntegral

isPrime :: (Integral n) => n -> Bool
isPrime n = (n >= 2) && all ((/= 0) . (n `mod`)) [2 .. isqrt n]
