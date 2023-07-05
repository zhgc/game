module Main where
-- import System.Random
import Lib ( guess_number )
import Games ( game1 )
import Hand (play_human_computer)
import Hangman (hangman)
main :: IO ()
main = manu 

manu :: IO ()
manu = do 
    putStrLn "选择游戏：\n1. 猜数游戏\n2. 井字棋\n3. 石头剪刀布\n4.猜数游戏\nexit. 退出"
    c <- getLine
    if c /= "exit"
        then do
            case c of
                "1" -> guess_number
                "2" -> game1
                "3" -> play_human_computer
                "4" -> hangman
                _   -> do
                    putStrLn "请重新选择"
                    manu
            putStrLn "是否继续游玩？y/n"
            c2 <- getLine
            case c2 of
                "y" -> manu
                "n" -> return ()
                _   -> manu
        else return ()