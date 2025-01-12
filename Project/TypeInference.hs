module TypeInference where
    import MyType
    import Term
    import TypeNaming
    import TypeSubstitutions

    type Context = Term -> Maybe MyType

    typeFind :: Term -> UsedTypes -> Context -> Substitutions -> (MyType, UsedTypes, Substitutions)
    typeFind term@(SingleTerm _) ut ctx subs =
        case ctx term of
            (Just t) -> (t, ut, subs)
            Nothing -> error ("No assuptions for type of variable " ++ show term ++ "!")

    typeFind term@(Application x y) ut ctx subs =
        case unifyType xType subs2 of
            Atom t ->
                if yType == xType
                    then error "The term has no type!"
                    else
                        let (res, ut3) = getNewTypeName ut2
                            xFuncType = Func yType (Atom res)
                        in (result xFuncType, ut3, addToFunction subs2 xType xFuncType)
            Func param res -> (res, ut2, if param == yType then subs2 else addToFunction subs2 param yType)
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

--TODO:
--support for fancy symbols
--support for terms with free variables