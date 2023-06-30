module Hangman(hangman) where

keyword = "haskell"
hangman = do 
    putStrLn ""
    ans <- getLine
    if ans == keyword
    then putStrLn "你猜对了"
    else do 
        putStrLn $ check ans keyword
        hangman

check :: String -> String -> String 
check a b 
    | null a || null b = []
check (a:as) (b:bs)
    | a == b = a:check as bs
    | otherwise = '-':check as bs
