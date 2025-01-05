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
        show (Func x@(Atom _) y) = show x ++ "->" ++ show y
        show (Func x@(Func _ _) y) = "(" ++ show x ++ ")" ++ "->" ++ show y

    instance Show Term where
        show :: Term -> String
        show (SingleTerm x) = [x]
        show (Application x y@(SingleTerm _)) = show x ++ show y
        show (Application x y) = show x ++ "(" ++ show y ++ ")"
        show (Lambda x y) = "\\" ++ [x] ++ "." ++ show y

    parameterType :: MyType -> MyType
    parameterType (Func x _) = x

    resultType :: MyType -> MyType
    resultType (Func _ y) = y

    separateBrackets :: String -> [String]
    separateBrackets str
        | '(' `elem` str = before ++ [inner] ++ separateBrackets rest
        | ')' `elem` str = error "Invalid syntax!"
        | otherwise = map (: []) str
        where
            splitOpenBracket :: String -> ([String], String)
            splitOpenBracket ('(':ys) = ([], ys)
            splitOpenBracket (y:ys) = ([y]:beforeStr, afterStr)
                where (beforeStr, afterStr) = splitOpenBracket ys

            (before, after) = splitOpenBracket str

            splitClosedBracket :: String -> (String, String)
            splitClosedBracket "" = error "Invalid syntax!"
            splitClosedBracket (')':ys) = ("", ys)
            splitClosedBracket (y:ys) = (y:beforeStr, afterStr)
                where (beforeStr, afterStr) = splitClosedBracket ys
            (inner, rest) = splitClosedBracket after

-- >>>separateBrackets "xy(asdf)as(a)"
-- ["x","y","asdf","a","s","a"]

    strToTerm :: String -> Term
    strToTerm "" = error "Invalid term syntax!"
    strToTerm [x] = SingleTerm x
    strToTerm ('\\':x:'.':xs) = Lambda x (strToTerm xs)
    strToTerm str = foldl1 Application $ map strToTerm $ separateBrackets str

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

    typeFind term@(SingleTerm _) ut ctx s =
        case ctx term of
            (Just t) -> (t, ut, ctx, s)
            Nothing -> error ("No assuptions for type of variable " ++ show term)

    typeFind term@(Application x y) ut ctx subs =
        case xType of
            Atom t ->
                let (res, ut1) = getNewTypeName ut
                    xFuncType = Func yType (Atom res)
                    newSubs :: Substitutions
                    newSubs v =
                        if v == xType
                            then Just xFuncType
                            else subs v
                in (resultType xFuncType, ut1, ctx, newSubs)
            Func p r ->
                let newSubs :: Substitutions
                    newSubs v =
                        if v == p
                            then Just yType
                            else subs v
                in (r, ut, ctx, newSubs)
        where
            (yType, _, _, _) = typeFind y ut ctx subs
            (xType, _, _, _) = typeFind x ut ctx subs

    typeFind term@(Lambda x y) ut ctx subs = (Func (unifyType xType newSubs) (unifyType yType newSubs), ut2, ctx, newSubs)
        where
            (xTypeName, ut1) = getNewTypeName ut
            xType = Atom xTypeName
            newCtx :: Context
            newCtx (SingleTerm x) = Just xType
            newCtx t = ctx t
            (yType, ut2, _, newSubs) = typeFind y ut1 newCtx subs

    lambdaTermTypeInherence :: Term -> MyType
    lambdaTermTypeInherence term@(Lambda _ _) = typeResult
        where
            ctx :: Context
            ctx _ = Nothing
            subs :: Substitutions
            subs _ = Nothing
            (typeResult, _, _, _) = typeFind term "" ctx subs

    --------------------TESTS-------------------------
    testTerm = SingleTerm 'x'
    --(testType, a, b) = typeFind testTerm "" defaultContext

-- >>> lambdaTermTypeInherence (strToTerm "\\x.xx")
-- ProgressCancelledException

--TODO:
--support for simpler lambdas (\xy.x)
--typeFind for lambdas
--countably infinite type names
