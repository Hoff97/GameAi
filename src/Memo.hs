module Memo where

import           Control.Applicative ((<|>))
import           Control.Monad.State
import qualified Data.Map            as M

type Memo a b = State (M.Map a b) b

memoizePure :: Ord a => (a -> b) -> a -> Memo a b
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

memoize :: Ord a => (a -> Memo a b) -> a -> Memo a b
memoize f x = do
    vals <- get
    case M.lookup x vals of
        Just res    -> return res
        _           -> computeAndInsert f x

memoizeAlt :: Ord a => (a -> Memo a b) -> (a -> [a]) -> a -> Memo a b
memoizeAlt f a x = do
    vals <- get
    case foldr ((<|>) . (`M.lookup` vals)) Nothing (x:a x) of
        Just res    -> return res
        _           -> computeAndInsert f x

computeAndInsert f x = do
    compute <- f x
    modify (M.insert x compute)
    return compute

runMemo :: State (M.Map a b) c -> c
runMemo x = evalState x M.empty
