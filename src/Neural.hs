module Neural where

import           Matrix

type NN = [Matrix Double]

createNN :: (Int -> Int -> Int -> Double) -> [Int] -> NN
createNN _ [] = []
createNN f n@(_:ns) = zipWith layer (zip [0..] n) ns
    where
        layer (i,a) b = createMatrix (f i) b a

data StepF = Linear | Treshhold | Sigmoid

stepF :: StepF -> Double -> Double
stepF Linear x = x
stepF Treshhold a = if a>=1 then 1 else 0
stepF Sigmoid x = 1/(1+(exp 1 ** (-x)))

evalNN :: StepF -> NN -> [Double] -> [[Double]]
evalNN f x v = foldl (\a b -> map (map $ stepF f) $ mult b a) (vect v) x
