import Data.List

group' :: [Int] -> [a] -> [[[a]]]
group' ln ls = map ((([([fst] <*>)] <*>) . groupBy grouper . sortBy sorter) . zip ls) (nub (permutations placers))
  where
    placers = concatMap (uncurry replicate) (zip ln [0 ..])
    sorter (_, r1) (_, r2) = r1 `compare` r2
    grouper (_, r1) (_, r2) = r1 == r2

group3 :: [a] -> [[[a]]]
group3 = group' [2, 3, 4]
