-- Simple recursive solution
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [s] = s
myLast (_ : tl) = myLast tl

-- Alternative Solution using fold
myLast' :: [a] -> a
myLast' = foldl1 (\_ x -> x)
