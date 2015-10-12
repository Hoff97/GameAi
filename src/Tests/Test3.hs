module Tests.Test3 where

import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map             as M

type Test a b = StateT (M.Map a b) (Writer [String]) b

comp :: Int -> Test Int Int
comp 0 = return 1
comp x = do
    vals <- get
    case M.lookup x vals of
        Just a  -> return a
        _       -> do
            tell ["Computing for: " ++ show x]
            r <- comp (x-1)
            let res = r*x
            modify (M.insert x res)
            return res

test = mapM comp [3,5,2]

a = runWriter $ runStateT test M.empty
