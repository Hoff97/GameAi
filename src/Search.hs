module Search where

distinct :: Eq a => [a] -> [a]
distinct = foldl (\a b -> if b `elem` a then a else b:a) []

placeQueens :: Int -> Int -> [[Int]]
placeQueens _ 0 = [[]]
placeQueens x d = do
    pos <- placeQueens x (d-1)
    let nums = filter (matches pos) [1..x]
    map (:pos) nums
        where
            matches p y = all (\(e,i) -> e /= y && abs (e-y) /= i) (zip p [1..])

queens :: Int -> [[Int]]
queens x = placeQueens x x
