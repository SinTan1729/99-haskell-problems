-- Simple recursive solution
myLength :: (Num n) => [a] -> n
myLength [] = 0
myLength (_ : tl) = 1 + myLength tl

-- Solution using fold
myLength' :: (Num n) => [a] -> n
myLength' = foldl (\acc _ -> acc + 1) 0
