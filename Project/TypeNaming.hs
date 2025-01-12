module TypeNaming where
    type UsedTypes = [String]

    typeNames :: [String]
    typeNames = map (:[]) "abcdefghijklmnopqrtuvwxyz"

    getNewTypeName :: UsedTypes -> (String, UsedTypes)
    getNewTypeName ut = getNewTypeNameHelper typeNames 1
        where
            getNewTypeNameHelper :: [String] -> Int -> (String, UsedTypes)
            getNewTypeNameHelper (x:xs) n =
                if x `elem` ut
                    then getNewTypeNameHelper xs n
                    else (x, x:ut)
            getNewTypeNameHelper _ n = getNewTypeNameHelper (map (++ show n) typeNames) (n + 1)