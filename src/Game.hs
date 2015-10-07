module Game where

import           Control.Monad.State
import           Data.List           (maximumBy, minimumBy)
import           Data.Ord            (comparing)

data Player = P1 | P2 deriving (Eq, Show)

class Game a where
    next :: Player -> a -> [a]

minPlay :: Game a => (a -> Double) -> Player -> Int -> a -> [a] --TODO: Add alpha-beta cutoff
minPlay f p 0 a = [minimumBy (comparing f) $ next p a]
minPlay f p x a = a:minimumBy (comparing (f . last)) nxt
    where
        nxt = map (maxPlay f nP (x-1)) $ next p a
        nP = if p == P1 then P2 else P1

maxPlay :: Game a => (a -> Double) -> Player -> Int -> a -> [a] --TODO: Add alpha-beta cutoff
maxPlay f p 0 a = [maximumBy (comparing f) $ next p a]
maxPlay f p x a = a:maximumBy (comparing (f . last)) nxt
    where
        nxt = map (minPlay f nP (x-1)) $ next p a
        nP = if p == P1 then P2 else P1

minD = -100000
maxD = 100000

alphaBeta :: Game a => Int -> (a -> Double) -> a -> [a] --TODO: Test
alphaBeta d f a = fst $ minMax a f P1 d minD maxD

minMax :: Game a => a -> (a -> Double) -> Player -> Int -> Double -> Double -> ([a],Double)
minMax a f _ 0 _ _ = ([a],f a)
minMax a f P1 i alpha beta = abPrune ([],minD) alpha beta (next P1 a)
    where
        abPrune s _ _ [] = s
        abPrune r1@(_,h1) a b (x:xs) = if b<alphaNew then maxi else abPrune maxi alphaNew b xs
            where
                r2@(v2,h2) = minMax x f P2 (i-1) a b
                maxi = if h1>h2 then r1 else r2
                alphaNew = max a (snd maxi)
minMax a f P2 i alpha beta = abPrune ([],maxD) alpha beta (next P2 a)
    where
        abPrune s _ _ [] = s
        abPrune r1@(_,h1) a b (x:xs) = if betaNew<alpha then mini else abPrune mini a betaNew xs
            where
                r2@(v2,h2) = minMax x f P1 (i-1) a b
                mini = if h1<h2 then r1 else r2
                betaNew = min b (snd mini)
