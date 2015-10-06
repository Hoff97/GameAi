module Test2 where

import           Control.Monad (replicateM)
import           Evolve
import           Rand

newtype N = N Double deriving Show

get :: N -> Double
get (N x) = x

instance Evolve N where
    rank = map (\x -> (x,1/(abs $ get x - 10)))
    mutate (N x) = do
        a <- getR
        return $ N $ x+(a*20-10)
    combine (N a) (N b) = return $ N $ (a+b)/2

test :: Rand [N]
test = do
    l <- replicateM 20 getR :: Rand [Double]
    let a = map (N . (*40)) l
    evolution 30 a
