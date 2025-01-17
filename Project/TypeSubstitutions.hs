module TypeSubstitutions where
    import MyType
    type Substitutions = MyType -> Maybe MyType

    unifyType :: Substitutions -> MyType -> MyType
    unifyType = unifyTypeHelper []
        where 
            unifyTypeHelper:: [String] -> Substitutions -> MyType -> MyType
            unifyTypeHelper typeHistory subs x@(Atom name) =
                if name `elem` typeHistory
                    then error "Recursive type definition detected. The term has no type."
                    else case subs x of
                        Just t -> unifyTypeHelper (name : typeHistory) subs t
                        Nothing -> x
            unifyTypeHelper typeHistory subs (Func x y) = Func (unifyTypeHelper typeHistory subs x) (unifyTypeHelper typeHistory subs y)

    addToFunction :: Eq a => (a -> Maybe b) -> (a, b) -> a -> Maybe b
    addToFunction f (param, res) x = if x == param then Just res else f x