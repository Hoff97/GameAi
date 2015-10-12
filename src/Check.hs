{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Check where

import           Control.Applicative ((<$>))
import qualified Data.Map            as M
import           Data.Maybe          (isJust)
import           Test.QuickCheck

newtype T = T [Int] deriving (Ord,Show,Arbitrary)

instance Eq T where
    (T a) == (T b) = a==b || a==reverse b

instance (Ord k,Arbitrary k,Arbitrary v) => Arbitrary (M.Map k v) where
    arbitrary = M.fromList <$> arbitrary

prop_map_insert :: M.Map T Int -> T -> Bool
prop_map_insert m l = isJust (M.lookup l inserted) && isJust (M.lookup reversed inserted)
    where
        inserted = M.insert l 0 m
        reversed = case l of T a -> T $ reverse a

test :: IO ()
test = quickCheck prop_map_insert
