import Control.Monad (replicateM)

-- There isn't much point in implementing the basic predicates by hand
-- as it'd be equivalent to just writing their truth tables
and' :: Bool -> Bool -> Bool
and' = (&&)

infixl 4 `and'`

or' :: Bool -> Bool -> Bool
or' = (||)

infixl 3 `or'`

nand' :: Bool -> Bool -> Bool
nand' x y = not $ and' x y

infixl 9 `nand'`

nor' :: Bool -> Bool -> Bool
nor' x y = not $ or' x y

infixl 9 `nor'`

xor' :: Bool -> Bool -> Bool
xor' = (/=)

infixl 9 `xor'`

impl' :: Bool -> Bool -> Bool
impl' x = or' (not x)

infixl 3 `impl'`

equ' :: Bool -> Bool -> Bool
equ' = (==)

infixl 8 `equ'`

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ putStrLn [toStr x ++ show (f x) | x <- replicateM n [True, False]]
  where
    toStr = unwords . map ((++) <$> show <*> space)
    space True = "   "
    space False = "  "
