data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

completeBinaryTreeNumbered :: Int -> Int -> Tree Int
completeBinaryTreeNumbered m n =
  Branch m branch1 branch2
  where
    branch1 =
      if 2 * m <= n
        then completeBinaryTreeNumbered (2 * m) n
        else Empty
    branch2 =
      if 2 * m + 1 <= n
        then completeBinaryTreeNumbered (2 * m + 1) n
        else Empty

useSameCharLabel :: Char -> Tree a -> Tree Char
useSameCharLabel _ Empty = Empty
useSameCharLabel c (Branch _ branch1 branch2) =
  Branch c branch1' branch2'
  where
    branch1' = useSameCharLabel c branch1
    branch2' = useSameCharLabel c branch2

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = useSameCharLabel 'x' $ completeBinaryTreeNumbered 1 n

isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree Empty = True
isCompleteBinaryTree (Branch _ _ Empty) = True
isCompleteBinaryTree (Branch _ branch1 branch2) =
  isCompleteBinaryTree branch1 && isCompleteBinaryTree branch2
