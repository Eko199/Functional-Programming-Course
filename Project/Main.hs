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
    unifyType x@(Atom t) subs =
        case subs x of
            Just t -> unifyType t subs
            Nothing -> x
    unifyType (Func x y) subs = Func (unifyType x subs) (unifyType y subs)

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
            Nothing -> error ("No assuptions for type of variable " ++ show term ++ "!")

    typeFind term@(Application x y) ut ctx subs =
        case xType of
            Atom t -> 
                if yType == xType 
                    then error "The term has no type!" 
                    else
                        let (res, ut3) = getNewTypeName ut2
                            xFuncType = Func yType (Atom res)
                            newSubs :: Substitutions
                            newSubs v =
                                if v == xType
                                    then Just xFuncType
                                    else subs2 v
                        in (resultType xFuncType, ut3, ctx, newSubs)
            Func p r ->
                let newSubs :: Substitutions
                    newSubs v =
                        if v == p
                            then Just yType
                            else subs2 v
                in (r, ut2, ctx, if p == yType then subs else newSubs)
        where
            (yType, ut1, _, subs1) = typeFind y ut ctx subs
            (xType, ut2, _, subs2) = typeFind x ut1 ctx subs1

    typeFind term@(Lambda x y) ut ctx subs = (Func (unifyType xType newSubs) (unifyType yType newSubs), ut2, ctx, newSubs)
        where
            (xTypeName, ut1) = getNewTypeName ut
            xType = Atom xTypeName
            newCtx :: Context
            newCtx tt@(SingleTerm t) = if t == x then Just xType else ctx tt
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

-------------------------TESTS-----------------------------
-- >>> lambdaTermTypeInherence (strToTerm "\\x.x")
-- a->a

-- >>> lambdaTermTypeInherence (strToTerm "\\x.xx")
-- The term has no type!

-- a -> (a -> b -> c) -> b -> c
-- >>> lambdaTermTypeInherence (strToTerm "\\y.\\x.\\z.xyz")
-- a->(a->c->e)->c->e

--TODO:
--support for simpler lambdas (\xy.x)
--typeFind for lambdas
--countably infinite type names
