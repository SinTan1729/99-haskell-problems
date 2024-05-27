data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x n =
  [Branch x l r | l <- minustwocase, r <- minusonecase]
    ++ [Branch x l r | l <- minusonecase, r <- minusonecase]
    ++ [Branch x l r | l <- minusonecase, r <- minustwocase]
  where
    minusonecase = hbalTree x (n - 1)
    minustwocase = hbalTree x (n - 2)
