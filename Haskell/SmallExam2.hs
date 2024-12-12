module SmallExam2 where
    --Problem 1
    -- histogram :: Eq a => [a] -> [(a, Int)]
    -- histogram [] = []
    -- histogram  [x] = [(x, 1)]
    -- histogram (x:xs) = addToHistogram
    --     where addToHistogram x ((y, count):hs) = 
    quicksort :: Ord a => [a] -> [a]
    quicksort [] = []
    quicksort (x:xs) = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]

    isInteresting :: Ord a => [a] -> Bool
    isInteresting l = isSortedInteresting 0 (quicksort l)
        where
            isSortedInteresting :: Eq a => Int -> [a] -> Bool
            isSortedInteresting _ [] = True
            isSortedInteresting _ [x] = True
            isSortedInteresting danger (x:y:ys) = (newDanger <= 1) && isSortedInteresting newDanger (y:ys)
                where newDanger = if x == y then danger + 1 else 0

-- >>> isInteresting [1,2,3,4]
-- >>> isInteresting [2,5,6,3,5,2,7,6]
-- >>> isInteresting [2,4,5,6,2,3,2]
-- True
-- True
-- False

    --Problem 2
    dups :: Ord a => [a] -> [a]
    dups l = helper (head $ quicksort l) (tail $ quicksort l)
        where
            helper _ [] = []
            helper n (x:xs) = if n == x then n : helper x xs else helper x xs

-- >>> dups [1,2,3,4]
-- >>> dups [2,5,6,3,5,2,7,6]
-- []
-- [2,5,6]

    --Problem 3
    allDups :: Ord a => [[a]] -> [a]
    allDups l = removeDups [x | x <- concat mapped, all (elem x) mapped]
        where
            mapped = map dups l
            removeDups l = helper (head $ quicksort l) (tail $ quicksort l)
                where
                    helper n [] = [n]
                    helper n (x:xs) =
                        if n == x
                            then helper x xs
                            else n : helper x xs


-- >>> allDups [[1,2,5,1,2],[6,5,2,1,3,5,2,6,1],[2,3,4,2,3]]
-- [2]

    --Problem 4
    furthestDups :: Ord a => [a] -> Int
    furthestDups l = maximum (mapFurthest l)
        where
            mapFurthest [] = []
            mapFurthest (x:xs) = furthest x 0 0 xs : mapFurthest xs
                where
                    furthest el curr currMax [] = currMax
                    furthest el curr currMax (y:ys) =
                        if el == y
                            then furthest el (curr + 1) (curr + 1) ys
                            else furthest el (curr + 1) currMax ys

-- >>> furthestDups [2,5,6,3,5,2,7,6]
-- 5

    minX l = helper [1..]
        where helper (x:xs) = if isInteresting (l++x) && (allDups l /= allDups (l++x)) then x else helper xs
