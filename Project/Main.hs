module Pain where
    data MyType = Atom Char | Func MyType MyType

    typeFind :: String -> MyType
    typeFind [x] = Atom 'a'
    typeFind ('\\':xs) = Atom 'a'
    typeFind term = Atom 'a'


--- >>> typeFind "xxxyxyx"
-- No instance for (Show MyType) arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_axb2
