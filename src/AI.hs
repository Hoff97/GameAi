module AI where

import           Control.Applicative ((<$>))
import           Evolve
import           Four
import           Neural
import           Rand
import           Util                (randomNN)

newtype FourNeural = FP NN

randomFourNeural :: Rand FourNeural
randomFourNeural = FP <$> randomNN [48,20,1]

instance Evolve FourNeural where
    rank = undefined  --FIXME
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
