module Memo where

import           Control.Monad.State

type Memo a b = State [(a,b)] b

memoize :: Eq a => a -> (a -> b) -> Memo a b
memoize i f = do
    vals <- get
    case lookup i vals of
        Just res    -> return res
        _           -> do
            let res = f i
            put $ (i,res):vals
            return res

col = collatz 0

--test2 = memoize

collatz :: Int -> Int -> Int
collatz c 1 = c
collatz c x = collatz (c+1) $ coll x

coll :: Int -> Int
coll x = if even x then x `div` 2 else x*3+1

colTest :: Int -> State [(Int,Int)] Int
colTest 1 = return 1
colTest x = do
    vals <- get
    case lookup x vals of
        Just res    -> return res
        _           -> do
            rC  <- colTest $ coll x
            modify ((x,rC+1):)
            return $ rC+1

testRow = mapM colTest [1..10000]

evCol :: State [(Int,Int)] a -> (a,[(Int,Int)])
evCol x = runState x []

runCol :: State [(Int,Int)] a -> a
runCol a = evalState a []
