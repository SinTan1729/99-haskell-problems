myButLast :: [a] -> a
myButLast [] = error "List length less than 2"
myButLast [a, _] = a
myButLast (hd : tl) = myButLast tl
