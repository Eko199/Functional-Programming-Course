module TypeNaming where
    import MyType
    import TypeSubstitutions (addToFunction)
    type UsedTypes = Int

    typeNames :: [String]
    typeNames = [c : x | x <- [] : map show [1..], c <- "abcdefghijklmnopqrtuvwxyz"]

    getNewTypeName :: UsedTypes -> String
    getNewTypeName = (typeNames !!)

    sortTypeNames :: MyType -> MyType
    sortTypeNames = (\(x, _, _) -> x) . sortTypeNamesHelper 0 (const Nothing)
        where
            sortTypeNamesHelper :: UsedTypes -> (String -> Maybe String) -> MyType -> (MyType, UsedTypes, String -> Maybe String)
            sortTypeNamesHelper ut mapping (Func x y) = (Func xType yType, ut2, mapping2)
                where
                    (xType, ut1, mapping1) = sortTypeNamesHelper ut mapping x
                    (yType, ut2, mapping2) = sortTypeNamesHelper ut1 mapping1 y

            sortTypeNamesHelper ut mapping (Atom x) = 
                case mapping x of 
                    Nothing -> let xTypeName = getNewTypeName ut in (Atom xTypeName, ut + 1, addToFunction mapping x xTypeName)
                    Just t -> (Atom t, ut, mapping)
