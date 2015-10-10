module Memo where

import           Control.Monad.State
import qualified Data.Map            as M

type Memo a b = State (M.Map a b) b

memoize :: Ord a => (a -> Memo a b) -> a -> Memo a b
memoize f x = do
    vals <- get
    case M.lookup x vals of
        Just res    -> return res
        _           -> do
            compute  <- f x
            modify (M.insert x compute)
            return compute

col = collatz 0

collatz :: Int -> Int -> Int
collatz c 1 = c
collatz c x = collatz (c+1) (coll x)

coll :: Int -> Int
coll x = if even x then x `div` 2 else x*3+1

colTest :: Int -> Memo Int Int
colTest = memoize co
    where
        co 1 = return 0
        co x = liftM (+1) . colTest . coll $ x

t = mapM colTest [1..100000]

runMemo :: State (M.Map a b) c -> c
runMemo x = evalState x M.empty
