dupli :: [a] -> [a]
dupli x = concat $ [replicate 2] <*> x
