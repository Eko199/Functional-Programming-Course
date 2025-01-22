module Seminar12 where
    import Control.Monad.State
    --Problem 1 in previous seminar
    --Problem 2
    data Tree a = Null | Node a (Tree a) (Tree a) deriving Show

    getIndex :: Eq a => [a] -> a -> Int
    getIndex [] _ = -1
    getIndex (x:xs) y
        | x == y = 0
        | y `notElem` xs = -1
        | otherwise = 1 + getIndex xs y

    labelTree :: Eq a => Tree a -> Tree Int
    labelTree tree = evalState (helper tree) []
        where
            helper :: Eq a => Tree a -> State [a] (Tree Int)
            helper Null = return Null
            helper (Node root left right) = do
                newLeft <- helper left
                rootIndex <- gets (`getIndex` root)
                modify (\visited -> if rootIndex == -1 then visited ++ [root] else visited)
                newRoot <- gets (\visited -> if rootIndex == -1 then length visited + 1 else rootIndex + 1)
                newRight <- helper right
                return $ Node newRoot newLeft newRight
                
-- >>> labelTree (Node 'c' (Node 'a' (Node 'b' Null Null) (Node 'a' Null Null)) (Node 'b' Null Null))
-- Node 4 (Node 3 (Node 2 Null Null) (Node 2 Null Null)) (Node 1 Null Null)
