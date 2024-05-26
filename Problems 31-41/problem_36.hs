import Data.List (find, group)

isqrt :: (Integral n) => n -> n
isqrt = floor . sqrt . fromIntegral

primeFactors :: (Integral n) => n -> [n]
primeFactors n =
  case find ((== 0) . (n `mod`)) [2 .. isqrt n] of
    Nothing -> [n]
    Just m -> m : primeFactors (n `div` m)

primeFactorsMult :: (Integral n) => n -> [(n, n)]
primeFactorsMult = map (\g -> (head g, fromIntegral $ length g)) . group . primeFactors
