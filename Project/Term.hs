module Term where
    data Term = SingleTerm Char | Application Term Term | Lambda Char Term deriving Eq
        
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

    isReservedSymbol :: Char -> Bool
    isReservedSymbol = (`elem` "().\\")

    strToTerm :: String -> Term
    strToTerm "" = error "Invalid term syntax!"
    strToTerm [x] = SingleTerm x
    strToTerm ('\\':x:'.':xs) = if isReservedSymbol x then error ("Invalid variable name " ++ [x]) else Lambda x (strToTerm xs)
    strToTerm ('\\':x:xs) =  if isReservedSymbol x then error ("Invalid variable name " ++ [x]) else Lambda x (strToTerm ('\\':xs))
    strToTerm str = foldl1 Application $ map strToTerm $ separateBrackets str

    instance Read Term where
        readsPrec :: Int -> ReadS Term
        readsPrec _ str = [(strToTerm str, "")]

    instance Show Term where
        show :: Term -> String
        show (SingleTerm x) = [x]
        show (Application x@(Lambda _ _) y@(SingleTerm _)) = "(" ++ show x ++ ")" ++ show y
        show (Application x@(Lambda _ _) y) = "(" ++ show x ++ ")" ++ "(" ++ show y ++ ")"
        show (Application x y@(SingleTerm _)) = show x ++ show y
        show (Application x y) = show x ++ "(" ++ show y ++ ")"
        show (Lambda x y) = "\\" ++ [x] ++ "." ++ show y
