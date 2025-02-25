data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch a Empty Empty) = [a]
leaves (Branch _ branch1 branch2) =
  leaves branch1 ++ leaves branch2
