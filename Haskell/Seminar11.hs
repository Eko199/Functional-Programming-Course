module Seminar11 where
    type Vertex = Int
    type Graph = [(Vertex, [(Vertex, Float)])]

    neighbours :: Graph -> Vertex -> [(Vertex, Float)]
    neighbours g vertex = head [n | (v, n) <- g, v == vertex]

    type PriorityQueue = [(Vertex, Float)]

    insertInPQ :: PriorityQueue -> (Vertex, Float) -> PriorityQueue
    insertInPQ [] x = [x]
    insertInPQ pq@(x@(xv, xdist):xs) el@(v, dist)
        | dist <= xdist && xv == v = el : newPq
        | dist <= xdist = el : x : newPq
        | xv == v = insertInPQ newPq el
        | otherwise = x : insertInPQ newPq el
        where newPq = filter ((/= v) . fst) xs

-- >>> insertInPQ [(1, 2.5), (2, 3.14)] (2, 0.7)
-- [(2,0.7),(1,2.5)]

-- >>> insertInPQ [(1, 2.5), (2, 3.14)] (2, 3.7)
-- [(1,2.5),(2,3.7)]

    dijkstra :: Graph -> Vertex -> Vertex -> Maybe Float
    dijkstra graph start end = helper [(start, 0)] []
        where
            helper :: PriorityQueue -> [Vertex] -> Maybe Float
            helper [] _ = Nothing
            helper ((current, dist):pq) visited =
                if current == end 
                    then Just dist
                    else helper 
                        (foldl insertInPQ pq 
                            [(name, dist + d) | (name, d) <- neighbours graph current, name `notElem` visited, name `notElem` map fst pq || dist + d < head [y | (x, y) <- pq, x == name] ])
                        (current:visited)

    dijkstraAll :: Graph -> Vertex -> [(Vertex, Float)]
    dijkstraAll graph start = helper [(start, 0)] []
        where
            helper :: PriorityQueue -> [Vertex] -> [(Vertex, Float)]
            helper [] _ = []
            helper (x@(current, dist):pq) visited =
                x : helper 
                        (foldl insertInPQ pq 
                            [(name, dist + d) | (name, d) <- neighbours graph current, name `notElem` visited, name `notElem` map fst pq || dist + d < head [y | (x, y) <- pq, x == name] ])
                        (current:visited)
