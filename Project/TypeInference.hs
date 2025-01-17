module TypeInference where
    import MyType
    import Term
    import TypeNaming
    import TypeSubstitutions

    type Context = Term -> Maybe MyType --the current assumptions of type bindings
    type ClosingArguments = [(Char, MyType)]

    typeFind :: Term -> UsedTypes -> Context -> Substitutions -> (MyType, UsedTypes, Substitutions, ClosingArguments)
    typeFind term@(Variable c) ut ctx subs =
        case ctx term of
            (Just t) -> (t, ut, subs, [])
            --Nothing -> error ("No assuptions for type of variable " ++ show term ++ "!")
            Nothing -> let t = Atom (getNewTypeName ut) in (t, ut + 1, subs, [(c, t)])

    typeFind term@(Application x y) ut ctx subs =
        case unifyType subs2 xType of
            t@(Atom _) -> let xFuncType = Func yType (Atom (getNewTypeName ut2))
                          in (result xFuncType, ut2 + 1, addToFunction subs2 (t, xFuncType), ca)
            Func param res -> 
                (res, ut2, 
                    if param == unifiedYType 
                        then subs2 
                        else let newSubsPairs = filter (uncurry (/=)) $ 
                                    case param of
                                        Atom _ -> [(param, yType)]
                                        Func paramParam paramRes ->
                                            case unifiedYType of
                                                Atom _ -> [(unifiedYType, param)]
                                                Func yParam yRes -> if paramParam == paramRes
                                                    then [(yParam, paramParam), (yRes, paramRes)]
                                                    else [(paramParam, yParam), (paramRes, yRes)]
                             in foldl addToFunction subs2 newSubsPairs, 
                ca)
        where
            (yType, ut1, subs1, ca1) = typeFind y ut ctx subs
            (xType, ut2, subs2, ca2) = typeFind x ut1 ctx subs1
            ca = ca2 ++ ca1
            unifiedYType = unifyType subs2 yType

    typeFind term@(Lambda x y) ut ctx subs = (Func xType yType, ut2, newSubs, ca)
        where
            xType = Atom (getNewTypeName ut)
            (yType, ut2, newSubs, ca) = typeFind y (ut + 1) (addToFunction ctx (Variable x, xType)) subs

    termTypeInference :: Term -> (MyType, ClosingArguments)
    termTypeInference term = (sortTypeNames $ unifyType subs $ foldr (Func . snd) typeResult ca, ca)
        where (typeResult, _, subs, ca) = typeFind term 0 (const Nothing) (const Nothing)

-- >>> termTypeInference (read "\\x.(\\f.f(fx))(\\y.x)" :: Term)
-- (a->a,[])

-- >>> termTypeInference (read "\\x.(\\y.xy)x" :: Term)
-- Recursive type definition detected. The term has no type.
