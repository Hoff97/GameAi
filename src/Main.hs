{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import           Four
import           Game
import           Test
import           Test2

main :: IO ()
main = play ((!!1) . alphaBeta 6 (heuristic . get)) (FWR start)
