module Game (game1) where
import System.Random
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
    
p2p :: ChessBoard -> IO ()
p2p boardList = do 
    c1 <- check boardList
    if c1
    then return ()
    else do
        putStr "先手棋子“O” "
        f <- getChess boardList
        let boardList2 = getBoardList boardList ("f",f)  -- 每次落子都要更新棋盘
        board boardList2
        c2 <- check boardList2
        if c2
        then return ()
        else do
            putStr "后手棋子“X” "
            s <- getChess boardList2
            let boardList3 = getBoardList boardList2 ("s",s) 
            board boardList3
            c3 <- check boardList3
            if c3
            then return ()
            else p2p boardList3

p2e :: ChessBoard -> String -> IO ()
p2e boardList playerChess = do
    c1 <- check boardList
    if c1
    then return ()
    else do
        f <- if playerChess == "O" then getChess boardList else robChess boardList "O"
        let boardList2 = getBoardList boardList ("f",f)
        c2 <- check boardList2
        board boardList2
        if c2
        then return ()
        else do
            s <- if playerChess == "O" then robChess boardList2 "X" else getChess boardList2
            let boardList3 = getBoardList boardList2 ("s",s)     
            board boardList3    
            c3 <- check boardList3        
            if c3        
            then return ()        
            else p2e boardList3 playerChess        
        

robChess :: ChessBoard -> String -> IO Int
robChess bl pc = do 
    let (flag,chess) = winMove bl pc -- (Bool,Int)
    if flag && rightMove bl chess
    then return chess
    else do
        let (flag2,chess2) = loseMove bl pc
        if flag2 && rightMove bl chess2
        then return chess2
        else do
            let(flag3,chess3) = inCorner bl
            if flag3 && rightMove bl chess3
            then return chess3
            else do 
                let (flag4,chess4) = inSpace bl
                if flag4 && rightMove bl chess4
                then return chess4
                else do 
                    let (flag5,chess5) = inEdge bl
                    if flag5 && rightMove bl chess
                    then return chess5
                    else return 0

winMove :: ChessBoard -> String -> (Bool,Int)
winMove bl pc = wrssh
    where
        abc = [(4,9,2),(3,5,7),(8,1,6),(4,3,8),(9,5,1),(2,7,6),(4,5,6),(2,5,8)]
        nb = zip bl [4,9,2,3,5,7,8,1,6]
        fsn = part nb
        nn = if pc == "O" then fst fsn else snd fsn
        rs = [[ (n,1::Int) | n <- nn ,n == a || n == b || n== c]  | (a,b,c) <- abc]
        w = map (\s -> unzip s ) rs
        wrs = map (\(s,x) -> if sum x == 2 then (True,15 - sum s) else (False,0)) w
        wrss = filter (\(b,_) -> b) wrs
        wrssr = filter (\(_,x) -> rightMove bl x) wrss
        wrssh = if null wrssr then (False,0) else head wrssr

loseMove :: ChessBoard -> String -> (Bool,Int)
loseMove bl pc = if pc == "O" then winMove bl "X" else winMove bl "O"

inCorner :: ChessBoard -> (Bool,Int)
inCorner bl = inCor bl ms  
    where 
        inCor :: ChessBoard -> [Int] -> (Bool,Int)
        inCor [] _ = (False,0)
        inCor _ [] = (False,0)
        inCor (b:bs) (m:mss) = if b == "1" || b == "3" || b == "7" || b == "9"  then (True,m) else inCor bs mss
        ms = [4,9,2,3,5,7,8,1,6]

inSpace :: ChessBoard -> (Bool,Int)
inSpace bl = inSpa  bl ms
    where 
        inSpa :: ChessBoard -> [Int] -> (Bool,Int)
        inSpa [] _ = (False,0)
        inSpa _ [] = (False,0)
        inSpa (b:bs) (m:mss) = if b == "5" then (True,m) else inSpa bs mss
        ms = [4,9,2,3,5,7,8,1,6]

inEdge :: ChessBoard -> (Bool,Int)
inEdge bl = inEdg bl ms
    where 
        inEdg :: ChessBoard -> [Int] -> (Bool,Int)
        inEdg [] _ = (False,0)
        inEdg _ [] = (False,0)
        inEdg (b:bs) (m:mss) = if b == "2" || b == "4" || b == "6" || b == "8" then (True,m) else inEdg bs mss
        ms = [4,9,2,3,5,7,8,1,6]
    -- board boardList

rightMove :: ChessBoard -> Int -> Bool
rightMove bl c = if (sum r::Int) >= 1 then False else True
    where
        r = [ 1| (x,y) <- z , y == c && (x == "O" || x == "X") ]
        z = zip bl [4,9,2,3,5,7,8,1,6]

check :: ChessBoard -> IO Bool
check bl
    | win fn = putStrLn "先手获胜" >> return True
    | win sn = putStrLn "后手获胜" >> return True
    | draw bl = putStrLn "和棋" >> return True
    | otherwise = return False
    where
        nb = zip bl [4,9,2,3,5,7,8,1,6]
        (fn,sn) = part nb

draw :: ChessBoard -> Bool
draw bl = if (sum [ if b /= "O" && b/= "X" then 1 else 0 | b <- bl ] :: Int) == 0 then True else False 

win :: [Int] -> Bool
win ns = if (sum w :: Int) == 15 then True else False
    where
        rs = [[ n | n <- ns ,n == a || n == b || n== c]  | (a,b,c) <- abc]
        abc = [(4,9,2),(3,5,7),(8,1,6),(4,3,8),(9,5,1),(2,7,6),(4,5,6),(2,5,8)]
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

type ChessBoard = [String]
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