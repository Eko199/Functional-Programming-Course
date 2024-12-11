module ExampleExam2 where
    --Problem 1
    quicksort :: Ord a => [a] -> [a]
    quicksort [] = []
    quicksort (x:xs) = quicksort [y | y <- xs, y < x ] ++ [x] ++ quicksort [y | y <- xs, y >= x]


    isNPerm :: Int -> (Int -> Int) -> Bool

    isNPerm n f = quicksort domain == quicksort [f x | x <- domain]
        where domain = [0..n-1]

-- >>> isNPerm 3 (\x -> (3 - x) `mod` 3)
-- >>> isNPerm 10 (`div` 2)
-- >>> isNPerm 10 (\x -> (x + 2) `mod` 10)
-- True
-- False
-- True

    notContains :: Eq a => [a] -> a -> Bool
    notContains l el = null [x | x <- l, x == el]

    contains :: Eq a => [a] -> a -> Bool
    contains l = not . notContains l

    --maxCycle :: Int -> (Int -> Int) -> [Int]
    maxCycle n f = fst $ withMaxLen [cycle [] x 0 | x <- [0..n-1]]
        where
            cycle :: [Int] -> Int -> Int -> ([Int], Int)
            cycle lst last len =
                if notContains lst last
                    then cycle (lst ++ [last]) (f last) (len + 1)
                    else (lst, len)
            withMaxLen :: [([Int], Int)] -> ([Int], Int)
            withMaxLen [] = ([], 0)
            withMaxLen (y@(x, len):xs) =
                if len >= tailLen
                    then y
                    else t
                where
                    t@(tail, tailLen) = withMaxLen xs

-- >>> maxCycle 3 (\x -> (3 - x) `mod` 3)
-- >>> maxCycle 10 (\x -> (x + 2) `mod` 10)
-- >>> maxCycle 10 (\x -> (x + 3) `mod` 10)
-- [1,2]
-- [0,2,4,6,8]
-- [0,3,6,9,2,5,8,1,4,7]

    --Problem 2
    movingAverage :: [Double] -> Int -> [Double]
    movingAverage l n = sum (take n l) / fromIntegral n : movingAverage (tail l) n

    nats :: [Double]
    nats = 0 : map (1+) nats
    test1 = [1076,1356,1918,6252,6766,5525] ++ nats
    test2 = [1076, 1356, 1918, 6252, 6766, 5525] ++ nats

-- >>> take 4 (movingAverage test1 3)
-- [1450.0,3175.3333333333335,4978.666666666667,6181.0]

    allAverages :: [Double] -> [[Double]]
    allAverages l = [movingAverage l n | n <- [2..]]

-- >>> take 3 (map (take 4) (allAverages test2))
-- [[1216.0,1637.0,4085.0,6509.0],[1450.0,3175.3333333333335,4978.666666666667,6181.0],[2650.5,4073.0,5115.25,4635.75]]

    --Problem 3
    allObjects :: [(String, [String])] -> [String]
    allObjects l = [x | x <- concatMap snd l, notContains (map fst l) x]

    inv = [ ("docs", ["ids", "invoices"]), ("ids", ["passport"]),  ("invoices", []), ("memes", []),
        ("family", ["new year", "birthday"]), ("funny", ["memes"]), ("pics", ["family", "funny"]) ]

-- >>> allObjects inv
-- ["passport","new year","birthday"]

    cleanUp :: [(String, [String])] -> [(String, [String])]
    cleanUp l = if null toClean then l else cleanUp $ cleaned l
        where
            content _ [] = []
            content x ((x1, y):xs) =
                if x == x1
                    then y
                    else content x xs
            toClean = [x | (x, []) <- l] ++ [item | (_, [item]) <- l, (y, _) <- l, y == item]
            cleaned [] = []
            cleaned ((label, items): xs) =
                if contains toClean label
                    then rest
                    else (label, filter (notContains toClean) items ++ concat [content x l | x <- items, contains toClean x, not $ null $ content x l]) : rest
                where rest = cleaned xs

-- >>> cleanUp inv
-- [("docs",["passport"]),("pics",["new year","birthday"])]
