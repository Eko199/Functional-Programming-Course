module Seminar9 where
    import Data.Maybe (fromMaybe, isNothing, isJust)

    --Problem 1
    safeHead :: [a] -> Maybe a
    safeHead [] = Nothing
    safeHead (x:_) = Just x

    safeTail :: [a] -> Maybe [a]
    safeTail [] = Nothing
    safeTail (_:xs) = Just xs

    safeUncons :: [a] -> Maybe (a, [a])
    safeUncons [] = Nothing
    safeUncons (x:xs) = Just (x, xs)

    stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
    stripPrefix l [] = Just l
    stripPrefix [] _ = Nothing
    stripPrefix (x:xs) (y:ys) = if x /= y then Nothing else stripPrefix xs ys

    findIndex :: Eq a => [a] -> a -> Maybe Int
    findIndex [] _ = Nothing
    findIndex (x:xs) y
        | x == y = Just 0
        | isNothing tailIndex = Nothing
        | otherwise = Just (1 + fromMaybe (-1) tailIndex)
        where tailIndex = findIndex xs y

    maybeToList :: Maybe [a] -> [a]
    maybeToList Nothing = []
    maybeToList (Just l) = l

    mapMaybe :: (a -> Maybe b) -> [a] -> [b]
    mapMaybe f l = [fromMaybe undefined (f x) | x <- l, isJust (f x)]

    --Problem 2
    data BinaryTree a = Null | BinaryTree a (BinaryTree a) (BinaryTree a) deriving Show

    maxSumPath :: BinaryTree Int -> Int
    maxSumPath Null = 0
    maxSumPath (BinaryTree v left right) = v + max (maxSumPath left) (maxSumPath right)

    --Problem 3
    prune :: BinaryTree a -> BinaryTree a
    prune Null = Null
    prune (BinaryTree v Null Null) = Null
    prune (BinaryTree v left right) = BinaryTree v (prune left) (prune right)

-- >>> prune (BinaryTree 1 (BinaryTree 2 Null Null) Null)
-- BinaryTree 1 Null Null

    --Problem 4
    bloom :: BinaryTree a -> BinaryTree a
    bloom Null = Null
    bloom leaf@(BinaryTree v Null Null) = BinaryTree v leaf leaf
    bloom (BinaryTree v left right) = BinaryTree v (bloom left) (bloom right)

-- >>> bloom (BinaryTree 1 (BinaryTree 2 Null Null) Null)
-- BinaryTree 1 (BinaryTree 2 (BinaryTree 2 Null Null) (BinaryTree 2 Null Null)) Null

    --Problem 5
    rotateRight :: BinaryTree a -> BinaryTree a
    rotateRight Null = Null
    rotateRight tree@(BinaryTree v Null _) = tree
    rotateRight (BinaryTree v (BinaryTree left leftLeft leftRight) right) = BinaryTree left leftLeft (BinaryTree v leftRight right)

-- >>> rotateRight (BinaryTree 50 (BinaryTree 17 (BinaryTree 9 Null Null) (BinaryTree 23 Null Null)) (BinaryTree 76 Null Null))
-- BinaryTree 17 (BinaryTree 9 Null Null) (BinaryTree 50 (BinaryTree 23 Null Null) (BinaryTree 76 Null Null))
    
    rotateLeft :: BinaryTree a -> BinaryTree a
    rotateLeft Null = Null
    rotateLeft tree@(BinaryTree v _ Null) = tree
    rotateLeft (BinaryTree v left (BinaryTree right rightLeft rightRight)) = BinaryTree right (BinaryTree v left rightLeft) rightRight

-- >>> rotateLeft (BinaryTree 17 (BinaryTree 9 Null Null) (BinaryTree 50 (BinaryTree 23 Null Null) (BinaryTree 76 Null Null)))
-- BinaryTree 50 (BinaryTree 17 (BinaryTree 9 Null Null) (BinaryTree 23 Null Null)) (BinaryTree 76 Null Null)

    --Problem 6
    treeMap :: (a -> b) -> BinaryTree a -> BinaryTree b
    treeMap _ Null = Null
    treeMap f (BinaryTree v left right) = BinaryTree (f v) (treeMap f left) (treeMap f right)

-- >>> treeMap (+1) (BinaryTree 17 (BinaryTree 9 Null Null) (BinaryTree 50 (BinaryTree 23 Null Null) (BinaryTree 76 Null Null)))
-- BinaryTree 18 (BinaryTree 10 Null Null) (BinaryTree 51 (BinaryTree 24 Null Null) (BinaryTree 77 Null Null))

    --Problem 7
    data Tree a = Empty | Tree a [Tree a] deriving Show

    instance Functor Tree where
        fmap _ Empty = Empty
        fmap f (Tree v children) = Tree (f v) (map (fmap f) children)

-- >>> fmap (*2) (Tree 1 [Tree 5 [Tree 12 [], Tree 3 []], Tree (-2) [], Tree 0 [], Tree 69 []])
-- Tree 2 [Tree 10 [Tree 24 [],Tree 6 []],Tree (-4) [],Tree 0 [],Tree 138 []]

    --Problem 8
    data NonEmpty a = Value a | a :+ (NonEmpty a) deriving Show
    infixr 5 :+

    head :: NonEmpty a -> a
    head (Value x) = x
    head (x :+ _) = x

    tail :: NonEmpty a -> NonEmpty a
    tail (Value x) = error "NonEmpty list has no tail!"
    tail (_ :+ t) = t

    length' :: NonEmpty a -> Int
    length' (Value _) = 1
    length' (_ :+ xs) = 1 + length' xs

    snoc :: NonEmpty a -> a -> NonEmpty a
    snoc (Value x) y = x :+ Value y
    snoc (x :+ xs) y = x :+ (xs `snoc` y)

    reverse' :: NonEmpty a -> NonEmpty a
    reverse' l@(Value x) = l
    reverse' (x :+ xs) = reverse' xs `snoc` x

    uncons :: NonEmpty a -> (a, NonEmpty a)
    uncons (Value x) = error "Cannot unconstruct a NonEmpty list of a single element!"
    uncons (x :+ xs) = (x, xs)

    map' :: (a -> b) -> NonEmpty a -> NonEmpty b
    map' f (Value x) = Value (f x)
    map' f (x :+ xs) = f x :+ map' f xs

    filter' :: (a -> Bool) -> NonEmpty a -> NonEmpty a
    filter' p (Value x) = if p x then Value x else error "Cannot filter a NonEmpty list to an empty list"
    filter' p (x :+ xs) = if p x then x :+ filter' p xs else filter' p xs

    foldr' :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr' op nv (Value x) = x `op` nv
    foldr' op nv (x :+ xs) = x `op` foldr' op nv xs

