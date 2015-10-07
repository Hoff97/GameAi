{-# LANGUAGE ScopedTypeVariables #-}

module Four where

import           Data.List                    (find)
import           Data.Maybe                   (isJust)
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding (Linear, start)
import           Game
import           Matrix                       (columns, diagonals, toVect)
import           Neural

data Field = Pl Player | Empty deriving (Eq,Show)

type FW = [[Field]]

newtype FWR = FWR FW deriving Show

instance Game FWR where
    next p (FWR a) = map FWR $ possibleMoves p a

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
        drawSquare (Pl P1) = square 1 <> circle 0.5 # fillColor yellow # scale 1
        drawSquare (Pl P2) = square 1 <> circle 0.5 # fillColor red # scale 1
        drawSquare _ = square 1

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

makeGame :: Bool -> NN -> NN -> FWR -> [FWR]
makeGame s a b f
    | gameEnd f     = []
    | otherwise     = (if s then switched else move) : makeGame (not s) b a switched
    where
        move = computeMove a f
        switched = switchP move
