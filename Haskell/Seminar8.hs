module Seminar8 where
    --Problem 1
    natPairs :: [(Integer, Integer)]
    natPairs = [(m n, n - floor (fromIntegral (m n) * (fromIntegral (m n) + 1) / 2)) | n <- [0..]]
        where m n = floor ((sqrt (8 * fromIntegral n + 1) - 1) / 2)

    natPairs2 :: [(Integer, Integer)]
    natPairs2 = [(x, y) | n <- [0..], y <- [0..n], x <- [0..n], x + y == n]

-- >>> elemIndex natPairs2
-- [(0,0),(1,0),(0,1),(2,0),(1,1)]

    --Problem 2
    compress :: Eq a => [a] -> [(a, Int)]
    compress [] = []
    compress [x] = [(x, 1)]
    compress (x:(y:ys))
        | x == y = (x, count + 1) : tail compressedTail
        | otherwise = (x, 1) : compressedTail
        where
            compressedTail = compress (y:ys)
            count = snd (head compressedTail)

-- >>> compress [1,1,2,3,3,3,4,2,2,2,2,1]
-- >>> compress "abba"
-- [(1,2),(2,1),(3,3),(4,1),(2,4),(1,1)]
-- [('a',1),('b',2),('a',1)]

    --Problem 3
    maxRepeated :: Eq a => [a] -> Int
    maxRepeated = maximum . map snd . compress

-- >>> maxRepeated [1,1,2,3,3,3,4,2,2,2,2,1,1]
-- 4

    --Problem 4
    makeSet :: Eq a => [a] -> [a]
    makeSet = foldr addSet []
        where
            addSet el [] = [el]
            addSet el (x:xs)
                | el == x = x:xs
                | otherwise = x : addSet el xs

-- >>> makeSet [1,1,2,3,3,3,4,2,2,2,1,1]
-- >>> makeSet "abba"
-- [1,2,4,3]
-- "ab"

    --Problem 5
    histogram :: Eq a => [a] -> [(a, Int)]
    histogram = foldr addDict []
        where
            addDict el [] = [(el, 1)]
            addDict el (x:xs)
                | el == fst x = (fst x, snd x + 1) : xs
                | otherwise = x : addDict el xs

-- >>> histogram [1,1,2,3,3,3,4,2,2,2,1,1]
-- [(1,4),(2,4),(4,1),(3,3)]

    --Problem 6
    maxDistance :: [(Double, Double)] -> Double
    maxDistance l =
        let dist (x1, y1) (x2, y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)
        in maximum [dist x y | x <- l, y <- l, x < y]

-- >>> maxDistance [(-1.1, 1), (1.8, 2), (3, 1), (-1, -2)]
-- 5.0

    --Problem 7
    quicksort :: (a -> a -> Bool) -> [a] -> [a]
    quicksort _ [] = []
    quicksort lesser (x:xs) = (quicksort lesser [y | y <- xs, lesser y x]) ++ [x] ++ quicksort lesser [y | y <- xs, not (lesser y x)]
-- >>> quicksort (<) [2,1, 3, 5, 4]
-- [1,2,3,4,5]

    specialSort :: Ord a => [[a]] -> [[a]]
    specialSort l = map fst (quicksort (\l1 l2 -> snd l1 < snd l2) [(x, biggestElement x) | x <- l])
        where
            moreHist (x, xcount) (y, ycount) = xcount > ycount || xcount == ycount && x > y
            biggestElement :: Ord a => [a] -> a
            biggestElement = fst . head . quicksort moreHist . histogram

-- >>> specialSort ["moo", "bee", "eve", "abracadabra", "abcdefg", "mama", "z"]
-- ["abracadabra","bee","eve","abcdefg","mama","moo","z"]

