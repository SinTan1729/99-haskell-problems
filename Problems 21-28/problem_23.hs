import Data.List
import System.Random

rndSelect :: [a] -> Int -> IO [a]
rndSelect ls n = do
  gen <- getStdGen
  return $ take n [ls !! m | m <- nub $ randomRs (0, length ls - 1) gen]
