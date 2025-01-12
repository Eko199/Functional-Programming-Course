module MyType where
    data MyType = Atom String | Func { parameter :: MyType,  result :: MyType } deriving Eq
    
    instance Show MyType where
        show :: MyType -> String
        show (Atom x) = x
        show (Func x@(Atom _) y) = show x ++ "->" ++ show y
        show (Func x@(Func _ _) y) = "(" ++ show x ++ ")" ++ "->" ++ show y