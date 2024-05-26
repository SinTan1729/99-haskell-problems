myGCD :: (Integral n) => n -> n -> n
myGCD m n =
  if remainder == 0
    then abs n
    else myGCD n remainder
  where
    remainder = m `mod` n

coprime :: (Integral n) => n -> n -> Bool
coprime m n = myGCD m n == 1

totient :: (Integral n) => n -> n
totient m = fromIntegral $ length $ filter (coprime m) [1 .. m - 1]
