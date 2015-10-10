module Examples.Collatz where

import           Control.Monad (liftM)
import           Memo

collatz :: Int -> Int
collatz x = if even x then x `div` 2 else x*3+1

collMemo :: Int -> Memo Int Int
collMemo = memoize co
    where
        co 1 = return 0
        co x = liftM (+1) . collMemo . collatz $ x
