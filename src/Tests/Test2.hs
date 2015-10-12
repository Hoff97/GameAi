module Tests.Test2 where

import           Four
import           Game
import           System.CPUTime

testPerf :: IO ()
testPerf = do
    putStrLn "Starting..."
    t <- mapM (computeTimed . test) [1..10]
    putStrLn ""
    putStrLn "Finished"
    print t
    let r = zipWith (flip (/)) t (tail t)
    print r

test x = alphaBeta x (heuristic 20 . get) (FWR start)

computeTimed :: Show a => a -> IO Double
computeTimed c = do
    a <- getCPUTime
    print c
    b <- getCPUTime
    return $ fromIntegral (b-a)/(10**12)
