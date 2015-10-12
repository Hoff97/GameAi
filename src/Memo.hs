module Memo where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import qualified Data.Map            as M

type MemoT a b m = StateT (M.Map a b) m b
type Memo a b = State (M.Map a b) b

memoizePure :: (Monad m,Ord a) => (a -> b) -> a -> MemoT a b m
memoizePure f = memoize (return . f)

-- REVIEW:Useful?
memoizeFeature :: Ord f => (a -> f) -> (a -> Memo f b) -> a -> Memo f b
memoizeFeature f func a = do
    vals <- get
    let feature = f a
    case M.lookup feature vals of
        Just res    -> return res
        _           -> do
            compute  <- func a
            modify (M.insert feature compute)
            return compute

memoize :: (Monad m,Ord a) => (a -> MemoT a b m) -> a -> MemoT a b m
memoize f x = do
    vals <- get
    case M.lookup x vals of
        Just res    -> return res
        _           -> computeAndInsert f x

memoizeAlt :: (Monad m,Ord a) => (a -> MemoT a b m) -> (a -> [a]) -> a -> MemoT a b m
memoizeAlt f a x = do
    vals <- get
    case foldr ((<|>) . (`M.lookup` vals)) Nothing (x:a x) of
        Just res    -> return res
        _           -> computeAndInsert f x

computeAndInsert f x = do
    compute <- f x
    modify (M.insert x compute)
    return compute

runMemo :: Monad m => StateT (M.Map a b) m c -> m c
runMemo x = evalStateT x M.empty

evalMemo :: Monad m => StateT (M.Map a b) m c -> m (c,M.Map a b)
evalMemo x = runStateT x M.empty
