module Exam2 where
    createSet :: Eq a => [a] -> [a]
    createSet [] = []
    createSet (x:xs) =
        if x `elem` xs
            then createSet xs
            else x : createSet xs

    --Problem 1
    points :: [(Integer, Integer)]
    points = [(x, y) | s <- [0..], x <- [-s..s], y <- [-s..s], s == round (sqrt $ fromIntegral (x ^ 2 + y ^ 2))]

-- >>> take 30 points
-- [(0,0),(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1),(-2,-1),(-2,0),(-2,1),(-1,-2),(-1,2),(0,-2),(0,2),(1,-2),(1,2),(2,-1),(2,0),(2,1),(-3,-1),(-3,0),(-3,1),(-2,-2),(-2,2),(-1,-3),(-1,3),(0,-3),(0,3)]
    
-- >>> elem (-5, -12) points
-- True
    
    --Can be done more easily by first creating a generator of all points and filtering them, but I figured this out too late
    --Solution during exam:
    --outsideCircle :: Int -> Int -> Int -> [(Int, Int)]
    --outsideCircle x y r = [(x1, y1) | s <- [0..], y1 <- [x-r-s..x+r+s], x1 <- [y-r-s..y+r+s], abs (x1 - x) + abs (y1 - y) == s, ((x - x1) ^ 2 + (y - y1) ^ 2) > r ^ 2]

    --Solution after exam:
    outsideCircle :: Int -> Int -> Int -> [(Integer, Integer)]
    outsideCircle x y r = [(x1, y1) | (x1, y1) <- points, (x - fromIntegral x1) ^ 2 + (y - fromIntegral y1) ^ 2 > r ^ 2]

-- >>> take 25 (outsideCircle 0 0 2)
-- [(-2,-1),(-2,1),(-1,-2),(-1,2),(1,-2),(1,2),(2,-1),(2,1),(-3,-1),(-3,0),(-3,1),(-2,-2),(-2,2),(-1,-3),(-1,3),(0,-3),(0,3),(1,-3),(1,3),(2,-2),(2,2),(3,-1),(3,0),(3,1),(-4,-2)]

-- >>> elem (3, 0) (outsideCircle 0 0 2)
-- True

-- >>> take 25 (outsideCircle 1 1 1)
-- [(1,-1),(0,0),(2,0),(-1,1),(3,1),(0,2),(2,2),(1,3),(1,-2),(0,-1),(2,-1),(-1,0),(3,0),(-2,1),(4,1),(-1,2),(3,2),(0,3),(2,3),(1,4),(1,-3),(0,-2),(2,-2),(-1,-1),(3,-1)]

    --Problem 3
    type Pilot = (String, Int, Int, Int)
    type Round = (String, [Pilot])
    type Season = [Round]

    season :: Season
    season = [("Silverstone", [("Leclerc", 15, 10, 82), ("Hamilton", 25, 5, 80), ("Verstappen", 18, 0, 78)]),
              ("Monza", [("Sainz", 18, 0, 81), ("Hamilton", 15, 10, 79), ("Verstappen", 25, 0, 77)]),
              ("Spa", [("Norris", 18, 5, 78), ("Russell", 25, 0, 76)])]

    pilotName :: Pilot -> String
    pilotName (name, _, _, _) = name

    winnerName :: Round -> String
    winnerName (_, x:xs) = pilotName (helper xs x)
        where
            helper [] best = best
            helper (curr@(currName, currPoints, _, _):ys) best@(bestName, bestPoints, _, _) =
                helper ys (if currPoints > bestPoints then curr else best)

    allWinners :: Season -> [String]
    allWinners = createSet . map winnerName

-- >>> allWinners season
-- ["Hamilton","Verstappen","Russell"]

    quicksort :: (a -> a -> Bool) -> [a] -> [a]
    quicksort _ [] = []
    quicksort lesser (x:xs) = quicksort lesser [y | y <- xs, y `lesser` x] ++ [x] ++ quicksort lesser [y | y <- xs, not (y `lesser` x)]

    allPilots :: Season -> [Pilot]
    allPilots = concatMap snd

    compressDict :: Eq a => [(a, Int)] -> [(a, Int)]
    compressDict = foldl addToDict []
        where
            addToDict [] p = [p]
            addToDict (x@(xk, xv):xs) curr@(key, value) =
                if xk == key
                    then (key, xv + value) : xs
                    else x : addToDict xs curr

    penaltyImpact :: Season -> [(String, Int)]
    penaltyImpact = quicksort (\ (_, x) (_, y) -> x > y) . compressDict . filter (\(_, penalty) -> penalty > 0) . map (\(n, _, p, _) -> (n, p)) . allPilots

-- >>> penaltyImpact season
-- [("Hamilton",15),("Leclerc",10),("Norris",5)]

    fastestLapArray :: [Pilot] -> Pilot
    fastestLapArray = head . quicksort (\ (_, _, _, x) (_, _, _, y) -> x < y)

    fastestLap :: Season -> String
    fastestLap = pilotName . fastestLapArray . allPilots

-- >>> fastestLap season
-- "Russell"

    missedFastestLapWins :: Season -> [String]
    missedFastestLapWins = map fst . filter (\r -> winnerName r /= pilotName (fastestLapArray $ snd r))

-- >>> missedFastestLapWins season
-- ["Silverstone"]
