module Game where

import           Control.Monad.State
import           Data.List           (maximumBy, minimumBy)
import           Data.Ord            (comparing)
import           Memo

data Player = P1 | P2 deriving (Eq, Show,Ord)

class Game a where
    next :: Player -> a -> [a]
    end :: a -> Bool
    alternative :: a -> [a]
    alternative _ = []

minD = -100000
maxD = 100000

first :: (a -> b) -> (a,c) -> (b,c)
first f (a,b) = (f a,b)

--TODO: Rewrite for Go
alphaBeta :: (Ord a,Game a) => Int -> (a -> Double) -> a -> [a]
alphaBeta d f a = fst $ runMemo $ minMax a f P1 d minD maxD

minMaxM :: (Ord a,Game a) => a -> (a -> Double) -> Player -> Int -> Double -> Double -> Memo a ([a],Double)
minMaxM pos heur player depth alpha beta = memoizeAlt (\x -> minMax x heur player depth alpha beta) alternative pos

minMax :: (Ord a,Game a) => a -> (a -> Double) -> Player -> Int -> Double -> Double -> Memo a ([a],Double)
minMax a f _ 0 _ _ = return ([a],f a)
minMax a f P1 i alpha beta
    | end a     = return ([a],f a)
    | otherwise = do
        r <- abPrune a ([],minD) alpha beta (next P1 a)
        return $ first (a:) r
    where
        abPrune prev s _ _ [] = return s
        abPrune prev r1@(_,h1) a b (x:xs) = do
            r2@(_,h2) <- minMaxM x f P2 (i-1) a b
            let maxi = if h1>h2 then r1 else r2
            let alphaNew = max a (snd maxi)
            if b<alphaNew then return maxi else abPrune prev maxi alphaNew b xs

minMax a f P2 i alpha beta
    | end a     = return ([a],f a)
    | otherwise = do
        r <- abPrune ([],maxD) alpha beta (next P2 a)
        return $ first (a:) r
    where
        abPrune s _ _ [] = return s
        abPrune r1@(_,h1) a b (x:xs) = do
            r2@(_,h2) <- minMaxM x f P1 (i-1) a b
            let mini = if h1<h2 then r1 else r2
            let betaNew = min b (snd mini)
            if betaNew<alpha then return mini else abPrune mini a betaNew xs
