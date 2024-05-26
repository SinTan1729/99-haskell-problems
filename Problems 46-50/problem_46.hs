-- There isn't much point in implementing the basic predicates by hand
-- as it'd be equivalent to just writing their truth tables
and' :: Bool -> Bool -> Bool
and' = (&&)

or' :: Bool -> Bool -> Bool
or' = (||)

nand' :: Bool -> Bool -> Bool
nand' x y = not $ and' x y

nor' :: Bool -> Bool -> Bool
nor' x y = not $ or' x y

xor' :: Bool -> Bool -> Bool
xor' = (/=)

impl' :: Bool -> Bool -> Bool
impl' x = or' (not x)

equ' :: Bool -> Bool -> Bool
equ' = (==)

table :: (Bool -> Bool -> Bool) -> IO ()
table f = mapM_ putStrLn [show x ++ " " ++ show y ++ " " ++ show (f x y) | x <- [True, False], y <- [True, False]]
