module Memo where

import           Control.Monad.State
import qualified Data.Map            as M

type Memo a b = State (M.Map a b) b

memoizePure :: Ord a => (a -> b) -> a -> Memo a b
memoizePure f = memoize (return . f)

memoize :: Ord a => (a -> Memo a b) -> a -> Memo a b
memoize f x = do
    vals <- get
    case M.lookup x vals of
        Just res    -> return res
        _           -> do
            compute  <- f x
            modify (M.insert x compute)
            return compute

runMemo :: State (M.Map a b) c -> c
runMemo x = evalState x M.empty
