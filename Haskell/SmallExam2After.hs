module SmallExam2After where
    --Problem 1
    quicksort :: Ord a => [a] -> [a]
    quicksort [] = []
    quicksort (x:xs) = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]

    isInteresting :: Ord a => [a] -> Bool
    isInteresting = isSortedInteresting 0 . quicksort
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
    dups = dupsSorted . quicksort
        where
            dupsSorted [] = []
            dupsSorted [x] = []
            dupsSorted (x:t@(y:xs)) = if x == y then x : rest else rest
                where rest = dupsSorted t

-- >>> dups [1,2,3,4]
-- >>> dups [2,5,6,3,5,2,7,6]
-- []
-- [2,5,6]

    --Problem 3
    removeDups :: Eq a => [a] -> [a]
    removeDups [] = []
    removeDups (x:xs) = x : removeDups [y | y <- xs, y /= x]

    allDups :: Ord a => [[a]] -> [a]
    allDups l = removeDups [x | x <- concat mapped, all (elem x) mapped]
        where mapped = map dups l

-- >>> allDups [[1,2,5,1,2],[6,5,2,1,3,5,2,6,1],[2,3,4,2,3]]
-- [2]

    --Problem 4
    furthestDups :: Ord a => [a] -> Int
    furthestDups = maximum . mapFurthest . (`zip` [0..])
        where
            mapFurthest [] = []
            mapFurthest ((x, i):xs) = (furthest x i xs - i) : mapFurthest xs
                where
                    furthest _ curr [] = curr
                    furthest el curr ((y, i):ys) = furthest el (if el == y then i else curr) ys

-- >>> furthestDups [2,5,6,3,5,2,7,6]
-- 5

    minNatToAdd :: (Num a, Enum a, Ord a) => [[a]] -> a
    minNatToAdd l = helper $ quicksort $ removeDups $ concatMap (filter (`elem` [1..])) l
        where
            helper [] = error "What???"
            helper (x:xs) =
                let newL = [lst ++ [x] | lst <- l] in
                    if isInteresting newL && (allDups l /= allDups newL)
                        then x
                        else helper xs

-- >>> minNatToAdd [[1,2,5,1,2],[6,5,2,1,3,5,2,6,1],[2,3,4,2,3]]
-- What???
-- >>> minNatToAdd [[1,2,5,1,2, 4],[6,5,2,1,3,4,5,2,6,1],[2,3,4,2,3]]
-- 4
