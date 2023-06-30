module Games(game1) where

import System.Random

-- 自定义类型
type ChessBoard = [String]

-- 全局常量
abc :: [(Int,Int,Int)]
abc = [(4,9,2),(3,5,7),(8,1,6),(4,3,8),(9,5,1),(2,7,6),(4,5,6),(2,5,8)]   -- 胜利条件

ms :: [Int]   -- 三阶幻方
ms = [4,9,2,3,5,7,8,1,6]  

-- 游戏主程序
game1 :: IO () 
game1 = do 
    putStrLn "选择游戏模式：\n1. 人机对战\n2. 人人对战"
    mode <- getLine
    case mode of
        "1" -> do
            putStrLn "猜先，1. 单 2. 双"
            c <- getLine
            let cn = read c:: Int
            r <- randomRIO (1,100) 
            let playerChess = if cn == r `mod`2 
                then "O"
                else "X"
            putStrLn ("您的棋子是"++playerChess)
            board ["1","2","3","4","5","6","7","8","9"]
            p2e ["1","2","3","4","5","6","7","8","9"] playerChess
        "2" -> do
            putStrLn "先手棋子O，后手棋子X"
            board ["1","2","3","4","5","6","7","8","9"]
            p2p ["1","2","3","4","5","6","7","8","9"]
        _   -> do
            putStr "输入错误，请重新"
            game1
    
-- 人人对战模式
p2p :: ChessBoard -> IO ()
p2p boardList = do 
    c1 <- check boardList
    if c1 /= " "
    then putStrLn (result c1)
    else do
        putStr "先手棋子“O” "
        f <- getChess boardList
        let boardList2 = getBoardList boardList ("f",f)  -- 每次落子都要更新棋盘
        board boardList2
        c2 <- check boardList2
        if c2 /= " "
        then putStrLn (result c2)
        else do
            putStr "后手棋子“X” "
            s <- getChess boardList2
            let boardList3 = getBoardList boardList2 ("s",s) 
            board boardList3
            c3 <- check boardList3
            if c3 /= " "
            then putStrLn (result c3)
            else p2p boardList3

-- 人机对战模式
p2e :: ChessBoard -> String -> IO ()
p2e boardList playerChess = do
    c1 <- check boardList
    if c1 /= " "
    then putStrLn (p2e_result c1 playerChess)
    else do
        f <- if playerChess == "O" then getChess boardList else robChess boardList "O"
        let boardList2 = getBoardList boardList ("f",f)
        c2 <- check boardList2
        board boardList2
        if c2 /= " "
        then putStrLn (p2e_result c2 playerChess)
        else do
            s <- if playerChess == "O" then robChess boardList2 "X" else getChess boardList2
            let boardList3 = getBoardList boardList2 ("s",s)     
            board boardList3    
            c3 <- check boardList3        
            if c3 /= " "     
            then putStrLn (p2e_result c3 playerChess)      
            else p2e boardList3 playerChess        

-- 输出棋局结果
result :: String -> String 
result winner = case winner of
        "O" -> "先手胜利" 
        "X" -> "后手获胜" 
        _   -> "两位棋手下成了平局"

-- 输出人机对战模式的棋局结果 ， 依赖result
p2e_result :: String -> String -> String
p2e_result winner player = case winner of
        "O" -> if player == "O" then  "恭喜你" else  "很遗憾"
        "X" -> if player == "X" then  "恭喜你" else  "很遗憾"
        _   -> ""
        ++ rs
        where 
            rs = result winner

-- 获得机器的移动步骤，仔细想一下，或许不需要IO，但getChess是IO Int，两者需要具有相同类型
-- 如果rob改成ChessBoard -> String -> Int ,那么getChess也应该包装成 ChessBoard -> Int 
robChess :: ChessBoard -> String -> IO Int
robChess bl pc = do 
    let (flag,chess) = winMove bl pc -- (Bool,Int)
    if flag 
    then return chess
    else do
        let (flag2,chess2) = loseMove bl pc
        if flag2
        then return chess2
        else do
            let(flag3,chess3) = inCorner bl
            if flag3 
            then return chess3
            else do 
                let (flag4,chess4) = inSpace bl
                if flag4 
                then return chess4
                else do 
                    let (flag5,chess5) = inEdge bl
                    if flag5 
                    then return chess5
                    else return 0

-- 用一个非IO类型的函数把getChess包装一下
-- 试了一下，发现不能这样做
-- humanChess :: ChessBoard -> Int
-- humanChess bl = (getChess bl) >>= \h 

winMove :: ChessBoard -> String -> (Bool,Int)
winMove bl pc
    | null w = (False,0)
    | otherwise = head w
    where
        (first,second) = part (zip bl ms)
        w = filter 
            (\(b,x) -> b && rightMove bl x) 
            (map 
                (\(s,x) -> if sum x == 2 then (True,15 - sum s) else (False,0)) 
                (map 
                    (\s -> unzip s ) 
                    [[ (n,1::Int) | 
                        n <- (if pc == "O" 
                              then first 
                              else second) ,n == a || n == b || n== c]  |
                             (a,b,c) <- abc]))

loseMove :: ChessBoard -> String -> (Bool,Int)
loseMove bl pc = if pc == "O" then winMove bl "X" else winMove bl "O"

inCorner :: ChessBoard -> (Bool,Int)
inCorner bl = in_f bl ms (\x -> x == "1" || x == "3" || x == "7" || x == "9") 

inSpace :: ChessBoard -> (Bool,Int)
inSpace bl = in_f  bl ms (\x -> x == "5")

inEdge :: ChessBoard -> (Bool,Int)
inEdge bl = in_f bl ms (\x -> x == "2" || x == "4" || x == "6" || x == "8")

in_f :: ChessBoard -> [Int] -> (String -> Bool) -> (Bool,Int)
in_f [] _ _ = (False,0)
in_f _ [] _ = (False,0)
in_f (b:bs) (m:mss) condition 
    | condition b = (True,m) 
    | otherwise = in_f bs mss condition

-- 检查移动是否可行
rightMove :: ChessBoard -> Int -> Bool
rightMove bl c = if (sum r::Int) >= 1 then False else True
    where
        r = [ 1| (x,y) <- z , y == c && (x == "O" || x == "X") ]
        z = zip bl [4,9,2,3,5,7,8,1,6]

check :: ChessBoard -> IO String
-- 其实应该用枚举？也许重新写一遍会好一些。
check bl
    | win fn = return "O"   -- 表示先手获胜
    | win sn = return "X"   -- 表示后手获胜
    | draw bl = return "E"      -- 表示和棋
    | otherwise = return " "                      -- 表示棋局继续走子
    where
        nb = zip bl [4,9,2,3,5,7,8,1,6]
        (fn,sn) = part nb

-- 是否和棋
draw :: ChessBoard -> Bool
draw bl = if (sum [ if b /= "O" && b/= "X" then 1 else 0 | b <- bl ] :: Int) == 0 then True else False 

-- 是否胜利
win :: [Int] -> Bool
win ns = if (sum w :: Int) == 15 then True else False
    where
        rs = [[ n | n <- ns ,n == a || n == b || n== c]  | (a,b,c) <- abc]
        -- abc = [(4,9,2),(3,5,7),(8,1,6),(4,3,8),(9,5,1),(2,7,6),(4,5,6),(2,5,8)]
        w = map (\s -> if sum s == 15 then 15 else 0) rs

part :: [(String,Int)] -> ([Int],[Int])
part [] = ([],[])
part ((c,n):nb)
    | c == "O" = (n:fn,sn)
    | c == "X" = (fn,n:sn)
    | otherwise = (fn,sn)
    where
        (fn,sn) = part nb

getBoardList :: ChessBoard -> (String,Int) -> ChessBoard
getBoardList bl (_,0) = bl
getBoardList bl pc = [ putChess xy pc | xy <- numBoard] 
    where 
        numBoard = zip bl [4,9,2,3,5,7,8,1,6]

putChess :: (String,Int) -> (String,Int) -> String
putChess (b,n) (p,c)
    | p == "f" && n == c = "O"
    | p == "s" && n == c = "X"
    | otherwise = b

board :: ChessBoard -> IO ()
board list = do
    putStrLn "+-----+-----+-----+"
    putStrLn ("|  " ++ list!!0 ++ "  |  " ++ list!!1 ++ "  |  " ++ list!!2 ++ "  |")
    putStrLn "+-----+-----+-----+"
    putStrLn ("|  " ++ list!!3 ++ "  |  " ++ list!!4 ++ "  |  " ++ list!!5 ++ "  |")
    putStrLn "+-----+-----+-----+"
    putStrLn ("|  " ++ list!!6 ++ "  |  " ++ list!!7 ++ "  |  " ++ list!!8 ++ "  |")
    putStrLn "+-----+-----+-----+"

getChess :: ChessBoard -> IO Int
getChess boardList = do 
    putStrLn "请输入棋子位置："
    c <- getLine
    let r = case c of
            "1" -> 4
            "2" -> 9
            "3" -> 2
            "4" -> 3
            "5" -> 5
            "6" -> 7
            "7" -> 8
            "8" -> 1
            "9" -> 6
            _   -> 0
    let right = sum [y| (x,y) <- zip boardList [4,9,2,3,5,7,8,1,6], x /= "O" && x /= "X" && y == r ]
    if right /= 0 
        then return right
        else do
            putStrLn "输入错误，请重试"
            rs <- getChess boardList
            return rs