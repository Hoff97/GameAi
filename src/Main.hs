{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           System.CPUTime
import           Tests.Test2

-- |The main Function
main :: IO ()
main = do
    putStrLn "Starting..."
    t <- mapM (computeTimed . test) [1..10]
    putStrLn ""
    putStrLn "Finished"
    print t
    let r = zipWith (flip (/)) t (tail t)
    print r

computeTimed :: Show a => a -> IO Double
computeTimed c = do
    a <- getCPUTime
    print c
    b <- getCPUTime
    return $ fromIntegral (b-a)/(10**12)
