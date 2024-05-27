data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show, Eq)

maxNodes :: Int -> Int
maxNodes = subtract 1 . (2 ^)

minNodesSeq :: [Int]
minNodesSeq = 0 : 1 : zipWith ((+) . (1 +)) minNodesSeq (tail minNodesSeq)

minNodes :: Int -> Int
minNodes = (!!) minNodesSeq

maxHeight :: Int -> Int
maxHeight n = subtract 1 $ length $ takeWhile (<= n) minNodesSeq

minHeight :: Int -> Int
minHeight = ceiling . logBase 2 . fromIntegral . (+ 1)

countNodes :: Tree a -> Int
countNodes Empty = 0
countNodes (Branch _ l r) = countNodes l + countNodes r + 1

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

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes _ 0 = [Empty]
hbalTreeNodes x n = concatMap (filter ((n ==) . countNodes) . hbalTree x) [minHeight n .. maxHeight n]
