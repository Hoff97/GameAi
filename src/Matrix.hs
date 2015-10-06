module Matrix where

type Matrix a = [[a]]

columns :: Matrix a -> Matrix a
columns = map reverse . foldl (zipWith (flip (:))) (repeat [])

mult :: Num a => Matrix a -> Matrix a -> Matrix a
mult a b = map (\line -> map (sum . zipWith(*) line) $ columns b) a

zipA :: (a -> b -> a) -> [a] -> [b] -> [a]
zipA _ a [] = a
zipA _ [] _ = []
zipA f (x:xs) (y:ys) = f x y:zipA f xs ys

diagonals :: Matrix a -> [[a]]
diagonals m = diagonalsR m ++ map reverse (diagonalsR (reverse m))

diagonalsR :: Matrix a -> [[a]]
diagonalsR [] = []
diagonalsR [a] = map (:[]) a
diagonalsR ([]:_) = []
diagonalsR ((a:as):xs) = [a] : zipA (flip (:)) (diagonalsR xs) as

vect :: [a] -> Matrix a
vect = map (:[])

toVect :: Matrix a -> [a]
toVect [] = []
toVect ([]:_) = []
toVect ((x:_):ys) = x:toVect ys

createMatrix :: (Int -> Int -> a) -> Int -> Int -> Matrix a
createMatrix f i j = map (\line -> map (f line) [0..j - 1]) [0..i - 1]
