{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Four
import           Game
import           Util

main :: IO ()
main = do
    gameCycle P1 start
    putStrLn "Done"

gameCycle :: Player -> FW -> IO FW
gameCycle p f = case won f of
    Just a -> do
        putStrLn $ "Player " ++ show a ++ " won."
        return f
    Nothing     ->
        if null moves then do
            putStrLn "Game ended as a draw"
            return f
        else do
            putStrLn $ "Enter a number between 0 and " ++ show (length moves - 1)
            n <- askC (\n -> n>=0 && n < length moves) :: IO Int
            putStrLn "Made move, writing to SVG file"
            draw4 (moves!!n)
            gameCycle next (moves!!n)
        where
            moves = possibleMoves p f
            next = if p == P1 then P2 else P1


draw4 :: FW -> IO ()
draw4 a = drawDiagram 400 400 $ toDiag a

askC :: Read a => (a -> Bool) -> IO a
askC f = do
    a <- readS
    if f a then return a else askC f
