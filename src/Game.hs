module Game where

import           Data.List (maximumBy, minimumBy)
import           Data.Ord  (comparing)

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
