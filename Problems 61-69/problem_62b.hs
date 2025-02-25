data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch a _ _) 1 = [a]
atLevel (Branch _ branch1 branch2) n =
  atLevel branch1 (n - 1) ++ atLevel branch2 (n - 1)
