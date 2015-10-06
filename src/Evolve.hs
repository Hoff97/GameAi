{-# LANGUAGE RankNTypes #-}

module Evolve where

import           Control.Monad (replicateM)
import           Rand

class Evolve a where
    rank :: [a] -> [(a,Double)]
    mutate :: a -> Rand a
    combine :: a -> a -> Rand a


evolution :: (Show a, Evolve a) => Int -> [a] -> Rand [a]
evolution 0 a = return a
evolution i a = do
    parents <- takeRandom a (length a `div` 2)
    kids <- mapM (uncurry combine) $ pair parents
    mutated <- mapM mutate (kids++a)
    let ranked = rank mutated
    selected <- replicateM (length a) $ select ranked
    evolution (i-1) selected

select :: [(a,Double)] -> Rand a
select l = do
    let s = sum $ map snd l
    a <- getR :: Rand Double
    return $ selectFrom l (a*s)

selectFrom :: [(a,Double)] -> Double -> a
selectFrom [(a,_)] _ = a
selectFrom ((a,n):xs) s = if s < n then a else selectFrom xs (s-n)

pair :: [a] -> [(a,a)]
pair (x:y:z) = (x,y):pair z
pair _ = []

takeRandom :: [a] -> Int -> Rand [a]
takeRandom _ 0 = return []
takeRandom [] _ = return []
takeRandom x n = do
    i <- getRR (0,length x - 1)
    r <- takeRandom x (n-1)
    return (x!!i : r)
