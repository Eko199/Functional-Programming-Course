module TypeSubstitutions where
    import MyType

    type Substitutions = MyType -> Maybe MyType

    unifyType :: Substitutions -> MyType -> MyType
    unifyType subs x@(Atom _) =
        case subs x of
            Just t -> unifyType subs t
            Nothing -> x
    unifyType subs (Func x y) = Func (unifyType subs x) (unifyType subs y)

    addToFunction :: Eq a => (a -> Maybe b) -> a -> b -> a -> Maybe b
    addToFunction f param res x = if x == param then Just res else f x