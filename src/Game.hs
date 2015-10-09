module Game where

import           Control.Monad.State
import           Data.List           (maximumBy, minimumBy)
import           Data.Ord            (comparing)

data Player = P1 | P2 deriving (Eq, Show)

class Game a where
    next :: Player -> a -> [a]
    end :: a -> Bool

minD = -100000
maxD = 100000

first :: (a -> b) -> (a,c) -> (b,c)
first f (a,b) = (f a,b)

alphaBeta :: Game a => Int -> (a -> Double) -> a -> [a]
alphaBeta d f a = fst $ minMax a f P1 d minD maxD

minMax :: Game a => a -> (a -> Double) -> Player -> Int -> Double -> Double -> ([a],Double) --TODO: Check for Doubles
minMax a f _ 0 _ _ = ([a],f a)
minMax a f P1 i alpha beta
    | end a     = ([a],f a)
    | otherwise = first (a:) $ abPrune a ([],minD) alpha beta (next P1 a)
    where
        abPrune prev s _ _ [] = s
        abPrune prev r1@(_,h1) a b (x:xs) = if b<alphaNew then maxi else abPrune prev maxi alphaNew b xs
            where
                r2@(_,h2) = minMax x f P2 (i-1) a b
                maxi = if h1>h2 then r1 else r2
                alphaNew = max a (snd maxi)

minMax a f P2 i alpha beta
    | end a     = ([a],f a)
    | otherwise = first (a:) $ abPrune ([],maxD) alpha beta (next P2 a)
    where
        abPrune s _ _ [] = s
        abPrune r1@(_,h1) a b (x:xs) = if betaNew<alpha then mini else abPrune mini a betaNew xs
            where
                r2@(_,h2) = minMax x f P1 (i-1) a b
                mini = if h1<h2 then r1 else r2
                betaNew = min b (snd mini)
