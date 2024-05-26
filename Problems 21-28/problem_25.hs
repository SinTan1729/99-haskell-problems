import Data.List (nub)
import System.Random (getStdGen, randomRs)

rndPermu :: [a] -> IO [a]
rndPermu ls = do
  gen <- getStdGen
  return $ take (length ls) [ls !! m | m <- nub $ randomRs (0, length ls - 1) gen]
