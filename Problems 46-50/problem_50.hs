import Data.List (sortBy)

data HTree a = HBranch Int (HTree a) (HTree a) | HLeaf Int a

hTreeVal :: HTree a -> Int
hTreeVal (HLeaf n _) = n
hTreeVal (HBranch n _ _) = n

toHTree :: [HTree Char] -> HTree Char
toHTree [t] = t
toHTree [HLeaf f1 l1, HLeaf f2 l2] =
  if f1 < f2
    then HBranch (f1 + f2) (HLeaf f1 l1) (HLeaf f2 l2)
    else HBranch (f1 + f2) (HLeaf f2 l2) (HLeaf f1 l1)
toHTree ls = toHTree (mod : tl)
  where
    (l1 : l2 : tl) = sortBy sorter ls
    sorter x y = hTreeVal x `compare` hTreeVal y
    mod = HBranch (hTreeVal l1 + hTreeVal l2) l1 l2

fromHTree :: HTree Char -> [(Char, String)]
fromHTree (HLeaf 100 a) = [(a, "0")]
fromHTree (HLeaf _ a) = [(a, "")]
fromHTree (HBranch _ t1 t2) = [(c, '0' : s) | (c, s) <- fromHTree t1] ++ [(c, '1' : s) | (c, s) <- fromHTree t2]

huffman :: [(Char, Int)] -> [(Char, String)]
huffman ls = map snd $ filter (\((c1, _), (c2, _)) -> c1 == c2) $ (,) <$> ls <*> sortedCodes
  where
    sortedCodes = fromHTree $ toHTree $ map (\(c, v) -> HLeaf v c) ls
