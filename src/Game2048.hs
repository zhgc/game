module Game2048 where
import Board (printBoard)
import System.Random (randomRIO)
import qualified Data.List as L
import Data.List (partition)
-- import GHC.IO.Handle (hGetChar)
-- import Data.Char (toLower)
-- import System.IO (stdin)

type Board = [[Int]]
data Operation = MoveLeft|MoveRight|MoveUp|MoveDown

pb :: Board -> IO ()
pb =putStr.(printBoard 4 4) 

startBoard :: Board
startBoard = take 4 (repeat $ take 4 (repeat 0))

moveRowLeft :: [Int] -> [Int]
moveRowLeft []  = []
moveRowLeft xs  = newright ++ newzero ++ left
    where
        (left,right) = partition (==0) xs
        newright = eval right
        newzero  = putZero (length right - length newright)

eval :: [Int] -> [Int]
eval [] = []
eval [x] = [x]
eval (x:y:xs)
    | x == y    = x+y:eval xs 
    | otherwise = x:eval (y:xs)

putZero :: Int -> [Int]
putZero 0 = []
putZero n = 0:putZero (n-1)    

moveBoard :: Operation -> Board -> Board
moveBoard MoveLeft  = map moveRowLeft
moveBoard MoveRight = map reverse . map moveRowLeft . map reverse
moveBoard MoveUp    = L.transpose . map moveRowLeft . L.transpose
moveBoard MoveDown  = L.transpose . moveBoard MoveRight . L.transpose

game2048Start :: IO () 
game2048Start = return startBoard >>= newTwo >>= newTwo >>= loop

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
            else if afterBoard == board
            then loop afterBoard
            else do 
                newBoard <- newTwo afterBoard
                if boardMoveable newBoard
                then loop newBoard
                else pb newBoard >> putStrLn "\n~~你输了~~">> return ()

win :: Board -> Bool
win board = 2048 `elem` (concat board)

boardMoveable :: Board -> Bool
boardMoveable board = or [(\xx ->board /= moveBoard xx board) x | x <- [MoveLeft,MoveRight,MoveUp,MoveDown]]

newTwo :: Board -> IO Board
newTwo board= do 
    x <- randomRIO (0,3)
    y <- randomRIO (0,3)
    if board!!x!!y == 0
    then return $ setNew x (setNew y 2 (board !! x)) board
    else newTwo board

setNew :: Int -> a -> [a] -> [a]
setNew n e s = take n s ++ [e] ++ drop (n + 1) s

toOperation :: String -> Maybe Operation
toOperation ('w':_) = Just MoveUp
toOperation ('a':_) = Just MoveLeft
toOperation ('s':_) = Just MoveDown
toOperation ('d':_) = Just MoveRight
toOperation _   = Nothing
