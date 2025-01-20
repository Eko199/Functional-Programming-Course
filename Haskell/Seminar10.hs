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

    --Problem 7
    data Direction = BSTLeft | BSTRight

    bstPath :: Ord a => a -> BST a -> Maybe [Direction]
    bstPath _ Empty = Nothing
    bstPath x (BST root left right) 
        | x == root = Just []
        | x < root = 
            case bstPath x left of
                Nothing -> Nothing
                Just path -> Just (BSTLeft : path)
        | otherwise = 
            case bstPath x right of
                Nothing -> Nothing
                Just path -> Just (BSTRight : path)

    --Problem 8
    data Expr = Var | Const Double | Add Expr Expr | Subtract Expr Expr | Multiply Expr Expr | Divide Expr Expr | Pow Expr Expr | Ln Expr deriving Show

    eval :: Expr -> Double -> Double
    eval Var d = d
    eval (Const d) _ = d
    eval (Add x y) d = eval x d + eval y d
    eval (Subtract x y) d = eval x d - eval y d
    eval (Multiply x y) d = eval x d * eval y d
    eval (Divide x y) d = eval x d / eval y d
    eval (Pow x y) d = eval x d ** eval y d
    eval (Ln x) d = log (eval x d)

    isFunction :: Expr -> Bool
    isFunction Var = True
    isFunction (Const _) = False
    isFunction (Add x y) = isFunction x || isFunction y
    isFunction (Subtract x y) = isFunction x || isFunction y
    isFunction (Multiply x y) = isFunction x || isFunction y
    isFunction (Divide x y) = isFunction x || isFunction y
    isFunction (Pow x y) = isFunction x || isFunction y
    isFunction (Ln x) = isFunction x

    derive :: Expr -> Expr
    derive Var = Const 1
    derive (Const _) = Const 0
    derive (Add x y) = Add (derive x) (derive y)
    derive (Subtract x y) = Subtract (derive x) (derive y)
    derive (Multiply x y) = Add (Multiply (derive x) y) (Multiply x (derive y))
    derive (Divide x y) = Divide (Subtract (Multiply (derive x) y) (Multiply x (derive y))) (Pow y (Const 2))
    derive e@(Pow x y) 
        | xFunc && yFunc = derive (Pow (Const (exp 1)) (Multiply y (Ln x)))
        | xFunc = if eval y 0 == 0 then Const 0 else Multiply y (Multiply (Pow x (Subtract y (Const 1))) (derive x))
        | otherwise = let a = eval x 0 in
            if a < 0 then error "Not supported" else
                if a == 0 then Const 0 else
                    Multiply e (Multiply (Ln (Const a)) (derive y))
        where
            xFunc = isFunction x
            yFunc = isFunction y
    derive (Ln x) = Divide (Const 1) Var

-- >>> derive (Pow (Multiply Var Var) (Add (Const 2) (Const 1)))
-- Multiply (Add (Const 2.0) (Const 1.0)) (Multiply (Pow (Multiply Var Var) (Subtract (Add (Const 2.0) (Const 1.0)) (Const 1.0))) (Add (Multiply (Const 1.0) Var) (Multiply Var (Const 1.0))))
