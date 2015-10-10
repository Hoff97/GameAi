module Memo where

import           Control.Monad.State
import qualified Data.Map            as M

type Memo a b = State (M.Map a b) b

col = collatz 0

collatz :: Int -> Int -> Int
collatz c 1 = c
collatz c x = collatz (c+1) (coll x)

coll :: Int -> Int
coll x = if even x then x `div` 2 else x*3+1

colTest :: Int -> State (M.Map Int Int) Int
colTest 1 = return 1
colTest x = do
    vals <- get
    case M.lookup x vals of
        Just res    -> return res
        _           -> do
            rC  <- colTest $ coll x
            modify (M.insert x $ rC+1)
            return $ rC + 1

testRow = mapM colTest [1..1000000]

evCol :: State (M.Map Int Int) a -> (a,M.Map Int Int)
evCol x = runState x M.empty

runCol :: State (M.Map Int Int) a -> a
runCol a = evalState a M.empty
