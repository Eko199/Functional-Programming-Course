module Seminar7 where

    --Problem 1
    complAdd :: (Double, Double) -> (Double, Double) -> (Double, Double)
    complAdd x y = (fst x + fst y, snd x + snd y)
    
    complSub :: (Double, Double) -> (Double, Double) -> (Double, Double)
    complSub x y = (fst x - fst y, snd x - snd y)

    complMul :: (Double, Double) -> (Double, Double) -> (Double, Double)
    complMul x y = (fst x * fst y - snd x * snd y, snd x * fst y + fst x * snd y)

-- >>> complAdd (1,2) (-3,5)
-- >>> complSub (4,8) (2,-1)
-- >>> complMul (3, 5) (2, 1)
-- (-2.0,7.0)
-- (2.0,9.0)
-- (1.0,13.0)

    --Interlude
    map' :: (a -> b) -> [a] -> [b]
    map' _ [] = []
    map' f (x:xs) = f x : map' f xs

    filter' :: (a -> Bool) -> [a] -> [a]
    filter' _ [] = []
    filter' p (x:xs)
        | p x = x : filter' p xs
        | otherwise = filter' p xs

    reverse'' :: [a] -> [a]
    reverse'' [] = []
    reverse'' (x:xs) = reverse'' xs ++ [x]

-- >>> reverse'' [1, 2, 3]
-- [3,2,1]

    length'' :: [a] -> Int
    length'' [] = 0
    length'' (_:xs) = 1 + length'' xs

    null' :: [a] -> Bool
    null' [] = True
    null' _ = False

    elem' :: Eq a => a -> [a] -> Bool
    elem' _ [] = False
    elem' y (x:xs) = x == y || elem' y xs

    take' :: Int -> [a] -> [a]
    take' _ [] = []
    take' 0 _ = []
    take' n (x:xs) = x : take' (n - 1) xs

    drop' :: Int -> [a] -> [a]
    drop' _ [] = []
    drop' 0 l = l
    drop' n (_:xs) = drop' (n - 1) xs

    zip' :: [a] -> [b] -> [(a, b)]
    zip' _ [] = []
    zip' [] _ = []
    zip' (x:xs) (y:ys) = (x, y) : zip xs ys

    zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith' _ _ [] = []
    zipWith' _ [] _ = []
    zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

    all'' :: (a -> Bool) -> [a] -> Bool
    all'' _ [] = True
    all'' p (x:xs) = p x && all'' p xs

    any'' :: (a -> Bool) -> [a] -> Bool
    any'' _ [] = False
    any'' p (x:xs) = p x || all'' p xs

    takeWhile' :: (a -> Bool) -> [a] -> [a]
    takeWhile' _ [] = []
    takeWhile' p (x:xs) 
        | p x = x : takeWhile' p xs
        | otherwise = []

    dropWhile' :: (a -> Bool) -> [a] -> [a]
    dropWhile' _ [] = []
    dropWhile' p (x:xs) 
        | p x = dropWhile' p xs
        | otherwise = x : xs

    --Problem 2
    distance :: (Double, Double) -> (Double, Double) -> Double
    distance (x1, y1) (x2, y2) = sqrt ( (x1 - x2) ** 2 + (y1 - y2) ** 2 )

-- >>> distance (0, 0) (1, 1)
-- >>> distance (-2,3) (1,7)
-- 1.4142135623730951
-- 5.0

    --Problem 3
    minimum' :: [Double] -> Double
    -- minimum' [x] = x
    -- minimum' (x:tail) 
    --     | x <= tailMin = x
    --     | otherwise = tailMin
    --     where tailMin = minimum' tail
    
    minimum' = foldr min (1 / 0)

-- >>> minimum' [4, 3, 1.2, 44, -1.1]
-- -1.1

    maximum' :: [Double] -> Double
    maximum' = foldr max (-1 / 0)

-- >>> maximum' [4, 3, 1.2, 44, -1.1]
-- 44.0

    reverse' :: [a] -> [a]
    reverse' = foldl addRev []
        where addRev l a = a : l

-- >>> reverse' [1, 2, 3, 4]
-- [4,3,2,1]

    length' :: [a] -> Int
    length' = foldl add1 0
        where add1 x _ = x + 1

-- >>> length' [1, 2, 3, 4]
-- 4

    all' :: (a -> Bool) -> [a] -> Bool
    all' p = foldr and' True
        where and' x = (p x &&)

-- >>> all even [1, 2, 4, 6]
-- False

    any' :: (a -> Bool) -> [a] -> Bool
    any' p = foldr or' True
        where or' x = (p x ||)

-- >>> any odd [2, 2, 4, 6]
-- False

    append' :: [a] -> [a] -> [a]
    append' l1 l2 = foldr (:) l2 l1

-- >>> append' [1, 2, 3, 4] [5, 6, 7]
-- [1,2,3,4,5,6,7]

    replicate' :: Int -> a -> [a]
    replicate' n x = foldr app [] [1..n]
        where app _ = (x:)

-- >>> replicate' 5 "aa"
-- ["aa","aa","aa","aa","aa"]

    --Problem 4
    countDiv :: Integer -> Int
    countDiv n = length [x | x <- [1..n], mod n x == 0]

-- >>> countDiv 12
-- 6
    sumDiv :: Int -> Int
    sumDiv n = sum [x | x <- [1..n], mod n x == 0]

-- >>> sumDiv 8
-- 15

    prime :: Integer -> Bool
    prime n = countDiv n == 2

-- >>> prime 1
-- False

    descartes :: [a] -> [b] -> [(a, b)]
    descartes l1 l2 = [(x, y) | x <- l1, y <- l2]

-- >>> descartes [1, 2, 3] ['a', 'b', 'c']
-- [(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c'),(3,'a'),(3,'b'),(3,'c')]

    --Problem 5
    primes :: [Integer]
    primes = [x | x <- [2..], prime x]

-- >>> take 7 primes
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541]

    --Problem 6
    primesEratosten :: [Integer]
    primesEratosten = sieve [2..]
        where sieve (x:xs) = x : sieve [n | n <- xs, mod n x /= 0]

-- >>> take 100 primesEratosten
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541]
    
    --Problem 7
    natPairs :: [(Integer, Integer)]
    natPairs = [(m n, n - floor (fromIntegral (m n) * (fromIntegral (m n) + 1) / 2)) | n <- [0..]]
        where m n = floor ((sqrt (8 * fromIntegral n + 1) - 1) / 2)

-- >>> take 10 natPairs
-- [(0,0),(1,0),(1,1),(2,0),(2,1),(2,2),(3,0),(3,1),(3,2),(3,3)]
