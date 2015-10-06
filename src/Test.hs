{-# LANGUAGE RankNTypes #-}

module Test where

import           Four
import           Rand
import           Util (drawDiagram, drawMatrix, randomNN, toMatrix)

test2 :: IO ()
test2 = do
    a <- runRandIO $ randomNN [48,20,1]
    b <- runRandIO $ randomNN [48,20,1]
    let c = makeGame False a b (FWR start)
    let d = drawMatrix $ toMatrix $ map (\(FWR g) -> toDiag g) c
    drawDiagram 400 400 d
    putStrLn "Drawn 1"
    _ <- getLine
    let e = makeGame False b a (FWR start)
    let f = drawMatrix $ toMatrix $ map (\(FWR g) -> toDiag g) e
    drawDiagram 400 400 f
    putStrLn "Drawn 2"

draw :: FW -> IO ()
draw a = drawDiagram 400 400 $ toDiag a
