import Data.List (nub)
import System.Random (getStdGen, randomRs)

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = do
  gen <- getStdGen
  return $ take n [[1 .. m] !! x | x <- nub $ randomRs (0, m - 1) gen]
