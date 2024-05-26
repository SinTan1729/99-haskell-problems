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

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show x ++ " " ++ show y ++ " " ++ show (x `f` y) | x <- [True, False], y <- [True, False]]
