data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n =
  if even (n - 1)
    then [Branch 'x' l r | l <- tree_even, r <- tree_even]
    else [Branch 'x' l r | l <- tree_odd1, r <- tree_odd2] ++ [Branch 'x' l r | l <- tree_odd2, r <- tree_odd1]
  where
    tree_even = cbalTree $ (n - 1) `div` 2
    tree_odd1 = cbalTree $ (n - 1) `div` 2
    tree_odd2 = cbalTree $ (n - 1) `div` 2 + 1
