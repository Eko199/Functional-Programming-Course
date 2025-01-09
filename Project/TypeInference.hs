module TypeInference where
    import System.IO (hFlush, stdout)

    data Term = SingleTerm Char | Application Term Term | Lambda Char Term deriving Eq
    data MyType = Atom String | Func { parameter::MyType,  result::MyType } deriving Eq
    type Substitutions = MyType -> Maybe MyType
    type Context = Term -> Maybe MyType
    type UsedTypes = [String]

    instance Show MyType where
        show :: MyType -> String
        show (Atom x) = x
        show (Func x@(Atom _) y) = show x ++ "->" ++ show y
        show (Func x@(Func _ _) y) = "(" ++ show x ++ ")" ++ "->" ++ show y

    instance Show Term where
        show :: Term -> String
        show (SingleTerm x) = [x]
        show (Application x@(Lambda _ _) y@(SingleTerm _)) = "(" ++ show x ++ ")" ++ show y
        show (Application x@(Lambda _ _) y) = "(" ++ show x ++ ")" ++ "(" ++ show y ++ ")"
        show (Application x y@(SingleTerm _)) = show x ++ show y
        show (Application x y) = show x ++ "(" ++ show y ++ ")"
        show (Lambda x y) = "\\" ++ [x] ++ "." ++ show y

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
            splitClosedBracket ('(':ys) = ('(':beforeStr ++ ")" ++ innerStr, restStr)
                where
                    (beforeStr, afterStr) = splitClosedBracket ys
                    (innerStr, restStr) = splitClosedBracket afterStr
            splitClosedBracket (')':ys) = ("", ys)
            splitClosedBracket (y:ys) = (y:beforeStr, afterStr)
                where (beforeStr, afterStr) = splitClosedBracket ys
            (inner, rest) = splitClosedBracket after

-- >>>separateBrackets "((\\y.y)(\\z.z))"
-- ["(\\y.y)(\\z.z)"]

    strToTerm :: String -> Term
    strToTerm "" = error "Invalid term syntax!"
    strToTerm [x] = SingleTerm x
    strToTerm ('\\':x:'.':xs) = Lambda x (strToTerm xs)
    strToTerm str = foldl1 Application $ map strToTerm $ separateBrackets str

    typeNames :: [String]
    typeNames = separateBrackets "abcdefghijklmnopqrtuvwxyz"

    getNewTypeName :: UsedTypes -> (String, UsedTypes)
    getNewTypeName ut = getNewTypeNameHelper typeNames 1
        where
            getNewTypeNameHelper :: [String] -> Int -> (String, UsedTypes)
            getNewTypeNameHelper (x:xs) n =
                if x `elem` ut
                    then getNewTypeNameHelper xs n
                    else (x, x:ut)
            getNewTypeNameHelper _ n = getNewTypeNameHelper (map (++ show n) typeNames) (n + 1)

    unifyType :: MyType -> Substitutions -> MyType
    unifyType x@(Atom t) subs =
        case subs x of
            Just t -> unifyType t subs
            Nothing -> x
    unifyType (Func x y) subs = Func (unifyType x subs) (unifyType y subs)

    addToFunction :: Eq a => (a -> Maybe b) -> a -> b -> a -> Maybe b
    addToFunction f param res x = if x == param then Just res else f x

    typeFind :: Term -> UsedTypes -> Context -> Substitutions -> (MyType, UsedTypes, Substitutions)

    typeFind term@(SingleTerm _) ut ctx s =
        case ctx term of
            (Just t) -> (t, ut, s)
            Nothing -> error ("No assuptions for type of variable " ++ show term ++ "!")

    typeFind term@(Application x y) ut ctx subs =
        case xType of
            Atom t ->
                if yType == xType
                    then error "The term has no type!"
                    else
                        let (res, ut3) = getNewTypeName ut2
                            xFuncType = Func yType (Atom res)
                        in (result xFuncType, ut3, addToFunction subs2 xType xFuncType)
            Func p r -> (r, ut2, if p == yType then subs else addToFunction subs2 p yType)
        where
            (yType, ut1, subs1) = typeFind y ut ctx subs
            (xType, ut2, subs2) = typeFind x ut1 ctx subs1

    typeFind term@(Lambda x y) ut ctx subs = (Func xType yType, ut2, newSubs)
        where
            (xTypeName, ut1) = getNewTypeName ut
            xType = Atom xTypeName
            (yType, ut2, newSubs) = typeFind y ut1 (addToFunction ctx (SingleTerm x) xType) subs

    termTypeInference :: Term -> MyType
    termTypeInference term = unifyType typeResult subs
        where (typeResult, _, subs) = typeFind term [] (const Nothing) (const Nothing)

    -- Използван модел: OpenAI, GPT-4o
    -- Запитване:
    -- > My putStr doesn't appear until after the getline is done
    -- Оригинален отговор:
    -- > import System.IO (hFlush, stdout)
    -- > main :: IO ()
    -- > main = do
    -- >     putStr "Enter your name: "
    -- >     hFlush stdout  -- Ensures "Enter your name: " is displayed immediately
    -- >     name <- getLine
    -- >     putStrLn ("Hello, " ++ name ++ "!")
    -- Направени промени:
    -- Използвани ред 1 и 5
    runApp :: IO MyType
    runApp = do
        putStr "Enter a lambda term: "
        hFlush stdout
        termTypeInference . strToTerm <$> getLine

    main :: IO ()
    main = do
        result <- runApp
        putStr ("The type of the term is: " ++ show result)

--TODO:
--support for simpler lambdas (\xy.x)
--support for fancy symbols
--support for terms with free variables