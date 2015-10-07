module AI where

import           Control.Applicative ((<$>))
import           Data.Functor.Sum
import           Data.Monoid
import           Evolve
import           Four
import           Game
import           Neural
import           Rand
import           Util                (randomNN)

newtype FourNeural = FP NN

getN :: FourNeural -> NN
getN (FP r) = r

randomFourNeural :: Rand FourNeural
randomFourNeural = FP <$> randomNN [48,20,1]

instance Monoid FourNeural where
    mempty = FP $ createNN (const $ const $ const 0) [48,20,1]
    mappend a _ = a


matchAll :: [a] -> [(a,a,Int,Int)]
matchAll [] = []
matchAll (x:xs) = zipWith comb xs [1..] ++ map rest (matchAll xs)
    where
        comb y i = (x,y,0,i)
        rest (a,b,i,j) = (a,b,i+1,j+1)

filterWith :: (a -> Maybe b) -> [a] -> [b]
filterWith f (x:xs) = case f x of
    Just a  -> a:filterWith f xs
    _       -> filterWith f xs

--TODO Prettify
combineWith :: Monoid c => [a] -> (a -> a -> b) -> (b -> Bool -> c) -> [c]
combineWith l f c = map collect [0..length l - 1]
    where
        collect i = mconcat $ filterWith (col i) combined
        col i (b,j,k)
            | i == j || i == k  = Just $ c b (i==j)
            | otherwise         = Nothing
        combined = map fed $ matchAll l
        fed (a,b,i,j) = (f a b,i,j)

-- TODO Test
instance Evolve FourNeural where
    rank n = zipWith conv n $ combineWith n (\a b -> won $ get $ last $ makeGame False (getN a) (getN b) (FWR start)) combine
        where
            conv r (Sum i) = (r,fromInteger i)
            combine (Just P1) b = if b then Sum 100 else Sum (-100)
            combine (Just P2) b = if b then Sum (-100) else Sum 100
            combine _ _ = Sum 0
    mutate (FP x) = do
        mutated <- mapM mutateMatr x
        return $ FP mutated
        where
            mutateMatr = mapM mutateLine
            mutateLine = mapM mutateCon
            mutateCon y = do
                d <- getR :: Rand Double
                return $ y+d
    combine (FP a) (FP b) = return $ FP $ zipWith matrixes a b
        where
            matrixes = zipWith lines
            lines = zipWith fields
            fields a b = (a+b)/2
