module MyType where
    data MyType = Atom String | Func { parameter :: MyType,  result :: MyType } deriving Eq
    
    instance Show MyType where
        show :: MyType -> String
        show (Atom x) = x
        show (Func x@(Atom _) y) = show x ++ "->" ++ show y
        show (Func x@(Func _ _) y) = "(" ++ show x ++ ")" ++ "->" ++ show y

    intersectTypes :: MyType -> MyType -> Bool
    intersectTypes (Atom x) (Atom y) = x == y
    intersectTypes x (Func y z) = intersectTypes x y || intersectTypes x z
    intersectTypes (Func x y) z = intersectTypes x z || intersectTypes y z