{-# LANGUAGE RankNTypes #-}

module Tests.Test1 where

import           AI
import           Control.Monad (replicateM, unless)
import           Data.List     (maximumBy, minimumBy)
import           Data.Ord      (comparing)
import           Evolve
import           Four
import           Game
import           Neural
import           Rand
import           Safe          (readMay)
import           Tests.Test2
import           Util          (drawDiagram, drawMatrix, randomNN, toMatrix)

--TODO: GUI for evolving/playing

draw :: FW -> IO ()
draw a = drawDiagram 400 400 $ toDiag a

readLnConstrained :: Read a => (a -> Bool) -> IO a
readLnConstrained c = do
    i <- getLine
    case readMay i of
        Just a  -> if c a then return a else readLnConstrained c
        _       -> readLnConstrained c

playTest :: IO ()
playTest = play ((!!1) . alphaBeta 8 (heuristic 20 . get)) (FWR start)

play :: (FWR -> FWR) -> FWR -> IO ()
play comp p@(FWR pos) = do
    putStrLn "Drawing current position..."
    draw pos
    let pM = possibleMoves P2 pos
    unless (null pM || end p) $ do
        putStrLn "Drawn. Please make your move"
        putStrLn $ "Input a number from 0 to " ++ show (length pM - 1)
        n <- readLnConstrained (\x -> x>=0 && x<length pM)
        putStrLn "Drawing your move"
        let nextPos = pM!!n
        draw nextPos
        unless(gameEnd $ FWR nextPos) $ do
            _ <- getLine
            putStrLn "Computer is thinking, please wait"
            let n = comp (FWR nextPos)
            putStrLn "Computer moved"
            play comp n

playAgainst = do
    let g = makeGame False player1 player2 (FWR start)
    let m = map (map toDiag) $ toMatrix (map get g)
    let diagram = drawMatrix m
    putStrLn "Computing, drawing..."
    drawDiagram 10000 10000 $ drawMatrix m
    putStrLn "Finished"
    where
        player1 = (!!1) . alphaBeta 8 (heuristic 20 . get)
        player2 = (!!1) . alphaBeta 8 (heuristic 20 . get)
