module Main where
    import Control.Monad ( when )
    import System.IO (hFlush, stdout)
    import MyType
    import Term
    import TypeInference

    -- Използван модел: OpenAI, GPT-4o
    -- Запитване:
    -- > My putStr doesn't appear until after the getline is done
    -- Оригинален отговор:
    -- > import System.IO (hFlush, stdout)
    -- > main :: IO ()
    -- > main = do
    -- >     putStr "Enter your name: "
    -- >     hFlush stdout  -- Ensures "Enter your name: " is displayed immediately
    -- >     name <- getLine
    -- >     putStrLn ("Hello, " ++ name ++ "!")
    -- Направени промени:
    -- Използвани ред 1 и 5
    runApp :: IO MyType
    runApp = do
        putStr "Enter a lambda term: "
        hFlush stdout
        line <- getLine
        let term = read line :: Term
        let (result, ca) = termTypeInference term
        when (ca /= []) $ putStrLn $ foldl (++) "The term has closed the free variables " (map ((:" ") . fst) ca) -- from IDE suggestion
            ++ "\nThe new term is " ++ show (foldr (Lambda . fst) term ca)
        return result

    main :: IO ()
    main = do
        result <- runApp
        putStr ("The type of the term is: " ++ show result)