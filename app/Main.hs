module Main where
-- import System.Random
import Lib ( guess_number )
import Games ( game1 )
import Hand (play_human_computer)
import Hangman (hangman)
import Csin (getsin)
main :: IO ()
main = manu 

manu :: IO ()
manu = do 
    putStrLn "选择游戏：\n1. 猜数游戏\n2. 井字棋\n3. 石头剪刀布\n4.猜数游戏\n5.测试使用c函数计算sin值exit. 退出"
    c <- getLine
    if c /= "exit"
        then do
            case c of
                "1" -> Lib.guess_number
                "2" -> Games.game1
                "3" -> Hand.play_human_computer
                "4" -> Hangman.hangman
                "5" -> getsin
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