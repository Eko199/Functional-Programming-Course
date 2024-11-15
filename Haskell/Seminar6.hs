module Seminar6 where

    --Problem 1
    fibonacci :: Integer -> Integer
    fibonacci 0 = 0
    fibonacci 1 = 1
    fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- >>> fibonacci 5
-- 5

-- >>> fibonacci 40
-- 102334155

-- >>> fibonacci 400
-- ProgressCancelledException

    --Problem 2
    fibonacci2 :: Integer -> Integer
    fibonacci2 n
        | n < 2 = n
        | otherwise = fibonacci2 (n - 1) + fibonacci2 (n - 2)

    phi :: Double
    phi = (1 + sqrt 5) / 2.0
    psi :: Double
    psi = 1 - phi

    fibonacci3 :: Integer -> Integer
    fibonacci3 n = ceiling((phi ** fromIntegral n - psi ** fromIntegral n) / sqrt 5)

    fibonacci4 :: Integer -> Integer
    fibonacci4 n = if n < 2 then n else fibonacci4(n - 1) + fibonacci4(n - 2)

    fibonacci5 :: Integer -> Integer
    fibonacci5 n
        | n < 2 = n
        | n >= 2 = fibHelper (n - 2) 0 1
        where
            fibHelper n last curr
                | n == 0 = last + curr
                | otherwise = fibHelper (n - 1) curr (last + curr)

-- >>> fibonacci2 5
-- 5
-- >>> fibonacci3 40
-- 102334155
-- >>> fibonacci3 400
-- 176023680645013774046881757732785534731657091705813432912407563117323492093864181760
-- >>> fibonacci5 400
-- 176023680645013966468226945392411250770384383304492191886725992896575345044216019675

    --Problem 3
    fastPow :: Double -> Int -> Double
    fastPow x n
        | n == 0 = 1
        | even n = half
        | odd n = x * half
        where
            half = fastPow x (div n 2) ** 2

-- >>> fastPow 2 4
-- 16.0

    --Problem 4
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

    --Problem 5
    distance :: (Double, Double) -> (Double, Double) -> Double
    distance x y = sqrt ((fst x - fst y) ** 2 + (snd x - snd y) ** 2)

-- >>> distance (-2,3) (1,7)
-- >>> distance (0,0) (1,1)
-- 5.0
-- 1.4142135623730951

    --Problem 6
    repeated :: (t -> t) -> Int -> (t -> t)
    repeated f 0 x = x
    repeated f n x = f (repeated f (n - 1) x)

-- >>> let foo = repeated succ 10 in foo 5
-- 15