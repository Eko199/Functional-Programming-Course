module TypeSubstitutions where
    import MyType

    type Substitutions = MyType -> Maybe MyType

    unifyType :: MyType -> Substitutions -> MyType
    unifyType x@(Atom _) subs =
        case subs x of
            Just t -> unifyType t subs
            Nothing -> x
    unifyType (Func x y) subs = Func (unifyType x subs) (unifyType y subs)

    addToFunction :: Eq a => (a -> Maybe b) -> a -> b -> a -> Maybe b
    addToFunction f param res x = if x == param then Just res else f x