{-# LANGUAGE RankNTypes #-}

module Test where

import           AI
import           Control.Monad (replicateM)
import           Data.List     (maximumBy, minimumBy)
import           Data.Ord      (comparing)
import           Evolve
import           Four
import           Game
import           Rand
import           Util          (drawDiagram, drawMatrix, randomNN, toMatrix)

test2 :: IO ()
test2 = do
    a <- replicateM 10 $ runRandIO $ randomNN [48,20,1]
    let b = map FP a
    res <- runRandIO $ evolution 30 b
    print $ map snd $ rank res
    putStrLn "Drawn 1"

draw :: FW -> IO ()
draw a = drawDiagram 400 400 $ toDiag a
