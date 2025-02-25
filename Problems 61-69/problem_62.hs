data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch a branch1 branch2) =
  (a : internals branch1) ++ internals branch2
