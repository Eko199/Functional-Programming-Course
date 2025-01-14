module TypeInference where
    import MyType
    import Term
    import TypeNaming
    import TypeSubstitutions

    type Context = Term -> Maybe MyType
    type ClosingArguments = [(Char, MyType)]

    typeFind :: Term -> UsedTypes -> Context -> Substitutions -> (MyType, UsedTypes, Substitutions, ClosingArguments)
    typeFind term@(SingleTerm c) ut ctx subs =
        case ctx term of
            (Just t) -> (t, ut, subs, [])
            --Nothing -> error ("No assuptions for type of variable " ++ show term ++ "!")
            Nothing -> let (t, ut1) = getNewTypeName ut in (Atom t, ut1, subs, [(c, Atom t)])

    typeFind term@(Application x y) ut ctx subs =
        case unifyType subs2 xType of
            Atom t ->
                if yType == xType
                    then error "The term has no type!"
                    else
                        let (res, ut3) = getNewTypeName ut2
                            xFuncType = Func yType (Atom res)
                        in (result xFuncType, ut3, addToFunction subs2 xType xFuncType, ca)
            Func param res -> (res, ut2, if param == yType then subs2 else addToFunction subs2 param yType, ca)
        where
            (yType, ut1, subs1, ca1) = typeFind y ut ctx subs
            (xType, ut2, subs2, ca2) = typeFind x ut1 ctx subs1
            ca = ca2 ++ ca1

    typeFind term@(Lambda x y) ut ctx subs = (Func xType yType, ut2, newSubs, ca)
        where
            (xTypeName, ut1) = getNewTypeName ut
            xType = Atom xTypeName
            (yType, ut2, newSubs, ca) = typeFind y ut1 (addToFunction ctx (SingleTerm x) xType) subs

    termTypeInference :: Term -> (MyType, ClosingArguments)
    termTypeInference term = (unifyType subs $ foldr (Func . snd) typeResult ca, ca)
        where (typeResult, _, subs, ca) = typeFind term [] (const Nothing) (const Nothing)

--TODO:
--support for fancy symbols