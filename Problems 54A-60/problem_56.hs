data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

isMirror :: Tree a -> Tree a -> Bool
isMirror Empty Empty = True
isMirror (Branch _ l1 r1) (Branch _ l2 r2) = isMirror l1 r2 && isMirror r1 l2
isMirror _ _ = False

symmetric :: (Eq a) => Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = isMirror l r
