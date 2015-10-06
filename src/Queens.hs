{-# LANGUAGE NoMonomorphismRestriction #-}

module Queens where

import           Search
import           Util                         (drawDiagram)

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude

data Field = P1 | Empty deriving Eq

type QBoard = [[Field]]

change :: [a] -> Int -> a -> [a]
change l i a = take i l ++ [a] ++ drop (i+1) l

toQBoard :: [Int] -> QBoard
toQBoard x = map line x
    where
        line a = change (map (const Empty) [1..length x]) (a-1) P1

xor :: Bool -> Bool -> Bool
xor a b
    | a==b = False
    | otherwise = True

drawQBoard :: QBoard -> Diagram B
drawQBoard x = foldl (===) mempty $ zipWith drawLine x $ cycle [True,False]
    where
        drawLine y e = foldl (|||) mempty $ zipWith drawSquare y $ cycle [True `xor` e,False `xor` e]
        drawSquare p b = (if p == P1 then circle 0.5 # fillColor grey # scale 0.7 else mempty)
            <> square 1 # fillColor (if b then black else white)
