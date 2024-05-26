import Data.List (find)

isqrt :: (Integral n) => n -> n
isqrt = floor . sqrt . fromIntegral

isPrime :: (Integral n) => n -> Bool
isPrime n = (n >= 2) && all ((/= 0) . (n `mod`)) [2 .. isqrt n]

goldbach :: (Integral n) => n -> (n, n)
goldbach n =
  if odd n
    then error "Odd number was given"
    else (p, n - p)
  where
    Just p = find (\p -> (n - p) `elem` primes) primes
    primes = filter isPrime [2 .. n - 1]
