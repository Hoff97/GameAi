module Tests.Test2 where

import           Control.Applicative ((<$>))
import           Four
import           Game
import           Matrix

win = 100000
loose = -win

allSubs :: FW -> [[Field]]
allSubs x = x ++ columns x ++ diagonals x

heuristic :: Double -> FW -> Double
heuristic m x = case won x of
    Just p  -> if p == P1 then win else loose
    _       -> if all (notElem Empty) x
        then 0
        else cols+m*threeM
            where
                threes = allSubs x >>= subs 3
                threeM = fromIntegral $ length (filter (all (==Pl P1)) threes) - length (filter (all (==Pl P2)) threes)
                cols = sum $ zipWith colsMapped [1,5,10,20,10,5,1] $ columns x
                colsMapped n c = n*sum (map fieldVal c)
                fieldVal (Pl P1) = 1
                fieldVal (Pl P2) = -1
                fieldVal _ = 0

test x = alphaBeta x (heuristic 20 . get) (FWR start)
