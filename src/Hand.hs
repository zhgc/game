module Hand(goHand, win, play_human_computer) where
import System.Random
    ( randomIO, Random(randomR, random), RandomGen )
import Data.Char (toUpper)

data Hand = Rock
          | Scissor 
          | Paper
    deriving (Show,Eq,Enum)

instance Random Hand where
    random :: RandomGen g => g -> (Hand, g)
    random g = case randomR (0,2) g of
        (r,g') ->  (toEnum r::Hand,g')

    randomR :: RandomGen g => (Hand, Hand) -> g -> (Hand, g)
    randomR (a,b) g = case randomR (fromEnum a,fromEnum b) g of 
        (r,g') -> (toEnum r::Hand,g')

goHand :: IO Hand
goHand = do 
    h <- getChar 
    case toUpper h of
        'R' -> return Rock
        'S' -> return Scissor
        'P' -> return Paper
        _   -> do
            putStrLn "输入错误，请输入R、S、P字符"
            r <- goHand
            return r

win ::Hand -> Hand -> Int 
win Rock Scissor  = 1
win Scissor Paper = 1
win Paper Rock    = 1
win Paper Scissor = -1
win Scissor Rock  = -1
win Rock Paper    = -1
win _ _           = 0

play_human_computer :: IO ()
play_human_computer = do
    putStrLn " 石头剪刀布，输入：R、S、P"
    c <- randomIO :: IO Hand
    h <- goHand 
    let k = win h c
    case k of
        1  -> putStrLn "恭喜，你赢了"
        -1 -> putStrLn "很遗憾，你输了"
        _  -> putStrLn "是和局"
    _ <- getChar
    putStr ""