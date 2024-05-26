myGCD :: (Integral n) => n -> n -> n
myGCD m n =
  if remainder == 0
    then abs n
    else myGCD n remainder
  where
    remainder = m `mod` n
