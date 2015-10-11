module Go where

import           Game
import           Matrix

data Field = Pl Player | Empty deriving (Eq,Show,Ord)

type FW = Matrix Field

newtype FWR = FWR FW deriving (Show,Eq,Ord)

instance Game FWR where
    next p (FWR a) = undefined
    end = undefined
