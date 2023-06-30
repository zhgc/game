module Hangman(hangman) where

keyword:: String
keyword = "haskell"

hangman:: IO ()
hangman = do 
    putStrLn "猜词游戏"
    ans <- getLine
    if ans == keyword
    then putStrLn "你猜对了"
    else do 
        putStrLn $ check ans keyword
        hangman

check :: String -> String -> String 
check _ [] = []
check [] (_:bs) = '-':check [] bs 
check (a:as) (b:bs)
    | a == b = a:check as bs
    | otherwise = '-':check as bs