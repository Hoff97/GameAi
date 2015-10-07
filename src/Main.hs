{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           AI
import           Evolve
import           Four
import           Game
import           Rand
import           Util   (drawDiagram, randomNN)

main :: IO ()
main = putStrLn "Test"

twice :: Monad m => m a -> m (a,a)
twice x = do
    a <- x
    b <- x
    return (a,b)

test :: IO ()
test = do
    (a,b) <- twice $ runRandIO $ randomNN [48,20,1]
    let c = makeGame False a b (FWR start)
    let d = toDiag $ get $ last c
    drawDiagram 400 400 d
    putStrLn "Drawn First"
    _ <- getLine
    let e = makeGame False b a (FWR start)
    let f = toDiag $ get $ last e
    drawDiagram 400 400 f
    putStrLn "Drawn Second"
    print $ map snd $ rank [FP a,FP b]
    putStrLn "Finished"
