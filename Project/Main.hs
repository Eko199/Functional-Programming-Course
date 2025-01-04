module Pain where
    data Term = SingleTerm Char | Application Term Term | Lambda Char Term
    data MyType = Atom Char | Func MyType MyType
    type Substitutions = MyType -> Maybe MyType
    type Context = Term -> Maybe MyType
    type UsedTypes = String

    instance Eq MyType where
        (==) :: MyType -> MyType -> Bool
        Atom x == Atom y = x == y
        Func t v == Func u s = t == u && v == s
        _ == _ = False

    instance Show MyType where
        show :: MyType -> String
        show (Atom x) = [x]
        show (Func x@(Atom x1) y) = show x ++ "->" ++ show y
        show (Func x@(Func x1 x2) y) = "(" ++ show x ++ ")" ++ "->" ++ show y

    parameterType :: MyType -> MyType
    parameterType (Func x _) = x

    resultType :: MyType -> MyType
    resultType (Func _ y) = y

    typeNames :: String
    typeNames = "abcdefghijklmnopqrtuvwxyz"

    getNewTypeName :: UsedTypes -> (Char, UsedTypes)
    getNewTypeName ut = getNewTypeNameHelper typeNames
        where 
            getNewTypeNameHelper :: String -> (Char, UsedTypes)
            getNewTypeNameHelper (x:xs) = 
                if x `elem` ut 
                    then getNewTypeNameHelper xs
                    else (x, x:ut)
            getNewTypeNameHelper _ = error "No more free type names!"

    unifyType :: MyType -> Substitutions -> MyType
    unifyType x@(Atom t) s = 
        case s x of
            Just t -> unifyType t s
            Nothing -> x
    unifyType (Func x y) s = Func (unifyType x s) (unifyType y s)

    typeFind :: Term -> UsedTypes -> Context -> Substitutions -> (MyType, UsedTypes, Context, Substitutions)

    -- If we can make new assumptions:
    -- typeFind term@(SingleTerm x) ut ctx s = 
    --     case ctx term of
    --         (Just t) -> (t, ut, ctx, s)
    --         Nothing -> 
    --             let (t, newUt) = getNewTypeName ut 
    --             in (unifyType (Atom t) s, newUt, ctx, s)

    typeFind term@(SingleTerm x) ut ctx s = 
        case ctx term of
            (Just t) -> (t, ut, ctx, s)
            Nothing -> error ("No assuptions for type of variable " ++ show term)

    -- typeFind term@(Application x y) ut ctx s = 
    --     case xType of
    --         Atom t -> 
    --             let (param, ut1) = getNewTypeName ut
    --                 (res, ut2) = getNewTypeName ut1
    --                 xFuncType = Func (Atom param) (Atom res)
    --                 newSubs :: Substitutions
    --                 newSubs xType = Just xFuncType
    --                 newSubs v = s v
    --             in (xFuncType, ut2, ctx, newSubs)

    --     where 
    --         (yType, _, _, _) = typeFind y ut ctx s
    --         (xType, _, _, _) = typeFind x ut ctx s

    --------------------TESTS-------------------------
    defaultContext :: Context
    defaultContext _ = Nothing

    testTerm = SingleTerm 'x'
    (testType, a, b) = typeFind testTerm "" defaultContext

-- >>> testType
-- a
