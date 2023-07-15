{-# LANGUAGE ParallelListComp #-}
module Game2048 where
import Board (printBoard)
import System.Random (randomRIO)
import qualified Data.List as L
import Data.List (partition)

type Board = [[Int]]
data Operation = MoveLeft|MoveRight|MoveUp|MoveDown

-- 获得打印棋盘的函数
pb :: Board -> IO ()
pb =putStr.(printBoard 4 4) 

-- 棋盘初始状态
startBoard :: Board
startBoard = take 4 (repeat $ take 4 (repeat 0))

-- 移动一行的数字，并且同时计算合并的结果
moveRowLeft :: [Int] -> [Int]
moveRowLeft []  = []
moveRowLeft xs  = newright ++ newzero ++ left
    where
        -- 将本行数字分为零和非零，在计算之后计算长度的变化来补零
        (left,right) = partition (==0) xs
        newright = eval right
        newzero  = putZero (length right - length newright)

-- 计算合并项
eval :: [Int] -> [Int]
eval [] = []
eval [x] = [x]
eval (x:y:xs)
    | x == y    = x+y:eval xs 
    | otherwise = x:eval (y:xs)

-- 用来补零的辅助函数
putZero :: Int -> [Int]
putZero 0 = []
putZero n = 0:putZero (n-1)    

-- 定义四个方向的移动函数。
moveBoard :: Operation -> Board -> Board
moveBoard MoveLeft  = map moveRowLeft
moveBoard MoveRight = map reverse . map moveRowLeft . map reverse
moveBoard MoveUp    = L.transpose . map moveRowLeft . L.transpose
moveBoard MoveDown  = L.transpose . moveBoard MoveRight . L.transpose

-- 循环
loop :: Board -> IO ()
loop board = do 
    pb board
    s <- getLine
    case toOperation s of
        Nothing -> putStrLn "输入错误" >> loop board
        Just op -> do
            let afterBoard = moveBoard op board
            if win afterBoard 
            then pb afterBoard >> putStrLn "\n！！你赢了！！" >> return ()
            -- 如果移动前后一样，就重复一次
            else if afterBoard == board
            then loop afterBoard
            else do 
                -- 新增了2之后，判断棋盘还有没有移动空间，
                newBoard <- newTwo afterBoard
                if boardMoveable newBoard
                then loop newBoard
                else pb newBoard >> putStrLn "\n~~你输了~~">> return ()

-- 判断是否胜利，有2048就获胜，先将二维列表展开成一维的
win :: Board -> Bool
win board = 2048 `elem` (concat board)

-- 判断棋盘有没有移动空间，用列表推导式生成对各种情况的判断，用or来判断其中是否有真值。
boardMoveable :: Board -> Bool
boardMoveable board = or [(\xx ->board /= moveBoard xx board) x | x <- [MoveLeft,MoveRight,MoveUp,MoveDown]]

-- 更新，提取出为零的瓷砖，不再进行重复的比较
newTwo :: Board -> IO Board
newTwo board= do 
    let zerolist = filter (\(_,_,v) -> v == 0) [(x,y,v)| xx <-board,v<-xx|x<-[0,1,2,3]::[Int] , y<-[0,1,2,3]::[Int]]
    xy <- randomRIO (0,length zerolist - 1)
    let (x,y,_) = zerolist!!xy
    return $ setNew x (setNew y 2 (board !! x)) board
        
-- newTwo的 辅助函数
setNew :: Int -> a -> [a] -> [a]
setNew n e s = take n s ++ [e] ++ drop (n + 1) s

-- 将用户输入转化为数据类型的枚举值
toOperation :: String -> Maybe Operation
toOperation ('w':_) = Just MoveUp
toOperation ('a':_) = Just MoveLeft
toOperation ('s':_) = Just MoveDown
toOperation ('d':_) = Just MoveRight
toOperation _   = Nothing

-- 游戏的主程序，在初始状态下添加两个2，然后进入循环
game2048Start :: IO () 
game2048Start = putStrLn "2048游戏，用wasd控制方向">> return startBoard >>= newTwo >>= newTwo >>= loop