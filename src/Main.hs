{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Examples.Collatz
import           Memo
import           System.CPUTime

main :: IO ()
main = do
    a <- getCPUTime
    let t = runMemo test
    print t
    b <- getCPUTime
    print $ fromIntegral (b-a)/(10**12)

test = do
    a <- mapM collMemo [500001,500003..1000000]
    return $ maximum a
