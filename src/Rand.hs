{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Rand where

import           Control.Monad.State
import           Data.Functor.Identity
import           System.Random

type RandT m a = RandomGen g => StateT g m a
type Rand a = RandT Identity a

runRand :: (Monad m ,RandomGen g) => RandT m a -> g -> m a
runRand x = evalStateT x

runRandTIO :: Monad m => RandT m a -> IO (m a)
runRandTIO x = do
    g <- newStdGen
    return $ runRand x g

runRandIO :: Rand a -> IO a
runRandIO = fmap runIdentity . runRandTIO

getR :: (Monad m,Random a) => RandT m a
getR = do
    a <- get
    let (r,g) = random a
    put g
    return r

getRR :: (Monad m,Random a) => (a,a) -> RandT m a
getRR range = do
    a <- get
    let (r,g) = randomR range a
    put g
    return r
