import Data.List (find)

isqrt :: (Integral n) => n -> n
isqrt = floor . sqrt . fromIntegral

primeFactors :: (Integral n) => n -> [n]
primeFactors n =
  case find ((== 0) . (n `mod`)) [2 .. isqrt n] of
    Nothing -> [n]
    Just m -> m : primeFactors (n `div` m)
