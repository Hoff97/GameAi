{-# LANGUAGE RankNTypes #-}

module Util where

import           Control.Monad                (replicateM, zipWithM)
import           Data.List                    (minimumBy)
import           Data.Ord                     (comparing)
import           Diagrams.Backend.SVG         (renderSVG)
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Matrix
import           Neural
import           Rand
import           Safe                         (readMay)
import           System.Random

drawDiagram :: Double -> Double -> Diagram B -> IO ()
drawDiagram w h = renderSVG "strukturine.svg" (mkSizeSpec2D (Just w) (Just h))

readS :: Read a => IO a
readS = do
    i <- getLine
    let a = readMay i
    case a of
        Just r  -> return r
        _       -> readS

gridSize :: Int -> (Int,Int)
gridSize i = minimumBy (comparing (\(a,b) -> abs (b-a))) $ filter (\(a,b) -> b*a - i < 6 && b*a-i>=0)
    [(a,b) | a <- [1..(ceiling $ sqrt $ fromIntegral i)], let b = i `div` a + if i `mod` a == 0 then 0 else 1]

toMatrix :: [a] -> Matrix a
toMatrix a = takeR cols a
    where
        (_,cols) = gridSize $ length a

takeR :: Int -> [a] -> Matrix a
takeR _ [] = []
takeR x y = take x y:takeR x (drop x y)

drawMatrix :: Matrix (Diagram B) -> Diagram B
drawMatrix = hsep 0.5 . map (vsep 0.5)

randomList :: Random a => Int -> Rand [a]
randomList x = replicateM x getR

randomMatrix :: Random a => Int -> Int -> Rand (Matrix a)
randomMatrix i j = replicateM i $ randomList j

randomNN :: [Int] -> Rand NN
randomNN [] = return []
randomNN n@(_:ns) = zipWithM layer n ns
    where
        layer a b = randomMatrix b a
