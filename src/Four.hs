{-# LANGUAGE ScopedTypeVariables #-}

module Four where

import           Data.List                    (find)
import           Data.Maybe                   (isJust)
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding (Linear, start)
import           Game
import           Matrix                       (columns, diagonals, toVect)
import           Neural

data Field = Pl Player | Empty deriving (Eq,Show,Ord)

type FW = [[Field]]

newtype FWR = FWR FW deriving (Show,Ord,Eq)

instance Game FWR where
    next p (FWR a) = map FWR $ possibleMoves p a
    end = gameEnd
    alternative (FWR a) = [FWR $ map reverse a]

get :: FWR -> FW
get (FWR r) = r

start :: FW
start = replicate 6 $ 7 `replicate` Empty

update2 :: Int -> Int -> a -> [[a]] -> [[a]]
update2 i j a l = update i updated l
    where
        updated = update j a (l!!i)

update :: Int -> a -> [a] -> [a]
update x a l = take x l ++ [a] ++ drop (x+1) l

placeAt :: Int -> Int -> Player -> FW -> Maybe FW
placeAt i j p f = if f !! i !! j == Empty then Just $ update2 i j (Pl p) f else Nothing

makeMove :: Player -> FW -> Int -> Maybe FW
makeMove p f j = foldl move Nothing [0..5]
    where
        move Nothing i = placeAt i j p f
        move ju _ = ju

toDiag :: FW -> Diagram B
toDiag = foldl (===) mempty . map drawLine . reverse
    where
        drawLine = foldl (|||) mempty . map drawSquare
        drawSquare (Pl P1) = square 1 # lineWidth 3 <> circle 0.5 # lineWidth 0 # fillColor yellow # scale 0.9
        drawSquare (Pl P2) = square 1 # lineWidth 3 <> circle 0.5 # lineWidth 0 # fillColor red # scale 0.9
        drawSquare _ = square 1 # lineWidth 3

possibleMoves :: Player -> FW -> [FW]
possibleMoves p f = foldl moveAt [] [0..6]
    where
        moveAt l j = case makeMove p f j of
            Just pos  -> pos:l
            _       -> l

allEq :: Eq a => [a] -> Bool
allEq [] = True
allEq [_] = True
allEq (a:b:r) = a==b && allEq (b:r)

subs :: Int -> [a] -> [[a]]
subs x l
    | length l < x  = []
    | length l == x = [l]
    | x == 0        = [[]]
    | x <= 2        = concatMap (\y -> [tail y, init y]) (subs (x+1) l)
    | otherwise     = concatMap (\y -> [tail y, init y]) (subs (x+1) l)

won :: FW -> Maybe Player
won f = case find cond allFours of
    Just (Pl x:_)   -> Just x
    _               -> Nothing
    where
        cond x = allEq x && Empty `notElem` x
        allFours = concatMap (subs 4) $ f ++ columns f ++ diagonals f

switchP :: FWR -> FWR
switchP (FWR a) = FWR $ map (map switch) a
    where
        switch (Pl P1) = Pl P2
        switch (Pl P2) = Pl P1
        switch x = x

toNNInput :: FW -> [Double]
toNNInput = concatMap (map field)
    where
        field Empty = 0
        field (Pl P1) = 1
        field (Pl P2) = -1

computeH :: NN -> FW -> Double
computeH n = head . toVect . evalNN Linear n . toNNInput

computeMove :: NN -> FWR -> FWR
computeMove n f = alphaBeta 2 (computeH n . get) f!!1
    where
        get (FWR a) = a

gameEnd :: FWR -> Bool
gameEnd (FWR a) = isJust (won a) || all (notElem Empty) a

makeGame :: Bool -> (FWR -> FWR) -> (FWR -> FWR) -> FWR -> [FWR]
makeGame s a b f
    | gameEnd f = []
    | otherwise     = (if s then switched else move) : makeGame (not s) b a switched
    where
        move = a f
        switched = switchP move

makeGameNN :: Bool -> NN -> NN -> FWR -> [FWR]
makeGameNN s a b = makeGame s (computeMove a) (computeMove b)

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
