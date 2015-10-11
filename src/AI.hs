module AI where

import           Control.Applicative ((<$>))
import           Data.Functor.Sum
import           Data.List           (minimumBy)
import           Data.Monoid
import           Data.Ord            (comparing)
import           Evolve
import           Four
import           Game
import           Neural
import           Rand
import           Util                (randomNN)

-- |
newtype FourNeural = FP NN deriving (Show,Read)

fromFile :: String -> IO FourNeural
fromFile = fmap read . readFile

toFile :: String -> FourNeural -> IO ()
toFile file = writeFile file . show

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
filterWith _ [] = []
filterWith f (x:xs) = case f x of
    Just a  -> a:filterWith f xs
    _       -> filterWith f xs

combineWith :: Monoid c => [a] -> (a -> a -> b) -> (b -> Bool -> c) -> [c]
combineWith l f c = map collect [0..length l - 1]
    where
        collect i = mconcat $ filterWith (col i) combined
        col i (b,j,k)
            | i == j || i == k  = Just $ c b (i==j)
            | otherwise         = Nothing
        combined = map fed $ matchAll l
        fed (a,b,i,j) = (f a b,i,j)

instance Evolve FourNeural where
    rank n = zipWith conv n $
        combineWith n twoGames combine
        where
            twoGames a b = (winner $ makeGame False (computeMove $ getN a) (computeMove $ getN b) (FWR start),winner $ makeGame False (computeMove $ getN b) (computeMove $ getN a) (FWR start))
            winner = won . get . last
            conv r (Sum i) = (r,fromInteger i)
            combine (Nothing,Nothing) _ = Sum 0
            combine (Just a, Just b) c
                | a==b      = Sum 100
                | a == P1   = if c then Sum 200 else Sum 0
                | otherwise = if c then Sum 0 else Sum 200
            combine (Nothing, Just a) b
                | a==P1     = if b then Sum 50 else Sum 150
                | otherwise = if b then Sum 150 else Sum 50
            combine (Just a, Nothing) b
                | a==P1     = if b then Sum 150 else Sum 50
                | otherwise = if b then Sum 50 else Sum 150
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
