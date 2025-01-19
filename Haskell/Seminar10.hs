module Seminar10 where
    --Problems 1-3 are from seminar 9
    --Problem 4
    data BST a = Empty | BST a (BST a) (BST a) deriving Show

    bstInsert :: Ord a => a -> BST a -> BST a
    bstInsert x Empty = BST x Empty Empty
    bstInsert x (BST root left right)
        | x <= root = BST root (bstInsert x left) right
        | otherwise = BST root left (bstInsert x right)

    bstSearch :: Ord a => a -> BST a -> Bool
    bstSearch _ Empty = False
    bstSearch x (BST root left right)
        | x == root = True
        | x < root = bstSearch x left
        | otherwise = bstSearch x right

    bstValues :: BST a -> [a]
    bstValues Empty = []
    bstValues (BST root left right) = bstValues left ++ [root] ++ bstValues right

    bstSize :: BST a -> Int
    bstSize Empty = 0
    bstSize (BST root left right) = 1 + bstSize left + bstSize right

    bstSort :: Ord a => [a] -> [a]
    bstSort = bstValues . foldr bstInsert Empty

    --Problem 5
    newtype KeyValuePair k v = KeyValuePair (k, v)

    instance (Eq k) => Eq (KeyValuePair k v) where
        (==) :: KeyValuePair k v -> KeyValuePair k v -> Bool
        KeyValuePair (k1, _) == KeyValuePair (k2, _) = k1 == k2

    instance (Ord k) => Ord (KeyValuePair k v) where
        (<=) :: KeyValuePair k v -> KeyValuePair k v -> Bool
        KeyValuePair (k1, _) <= KeyValuePair (k2, _) = k1 <= k2

    newtype Map k v = Map (BST (KeyValuePair k v))

    bstInsertReplace :: Ord a => a -> BST a -> BST a
    bstInsertReplace x Empty = BST x Empty Empty
    bstInsertReplace x (BST root left right)
        | x < root = BST root (bstInsertReplace x left) right
        | x == root = BST x left right
        | otherwise = BST root left (bstInsertReplace x right)

    mapInsert :: Ord k => k -> v -> Map k v -> Map k v
    mapInsert k v (Map tree) = Map (bstInsertReplace (KeyValuePair (k, v)) tree)

    mapSearch :: Ord k => k -> Map k v -> Maybe v
    mapSearch _ (Map Empty) = Nothing
    mapSearch x (Map (BST (KeyValuePair (k, v)) left right))
        | x == k = Just v
        | x < k = mapSearch x (Map left)
        | otherwise = mapSearch x (Map right)

    --Problem 6
    instance Functor (KeyValuePair k) where
        fmap :: (a -> b) -> KeyValuePair k a -> KeyValuePair k b
        fmap f (KeyValuePair (k, v)) = KeyValuePair (k, f v)

    instance Functor (Map k) where
        fmap :: (a -> b) -> Map k a -> Map k b
        fmap _ (Map Empty) = Map Empty
        fmap f (Map (BST root left right)) = Map (BST (fmap f root) leftTree rightTree)
            where 
                (Map leftTree) = fmap f (Map left)
                (Map rightTree) = fmap f (Map right)