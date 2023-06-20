module Lib
    ( someFunc
    , getRandomInt
    , guess
    , guess_number
    , throwDice
    , throw_many_times
    , freq_of_point
    , dice_freq
    , dice_point_frequencies
    , average
    , dice_point_pro
    , rep
    , throw6Dice
    , atleast_on_point_fre
    , randomPoint
    , inCircleFre
    , compute_pi
    ) where
import System.Random (randomIO, randomRIO)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getRandomInt :: IO () 
getRandomInt = do
    r <- System.Random.randomIO :: IO Int
    print r

guess :: Int -> Int -> IO ()
guess answer count = do 
    putStrLn "猜数(1~100)"
    n <- getLine
    let m = read n:: Int
    if m == answer then 
        putStrLn ("很好！很有精神！"++"你猜了"++show count++"次")
    else if m < answer then do
        putStrLn "再大点"
        guess answer (count + 1)
    else do
        putStrLn "再小点"
        guess answer (count + 1)

guess_number :: IO ()
guess_number = do 
    some_number <-System.Random.randomIO :: IO Int
    let answer = some_number `mod` 101
    guess answer 1

throwDice :: IO Int
throwDice = do
    n <- System.Random.randomIO :: IO Int
    return (n `mod` 6 + 1)

throw_many_times :: Int -> IO [Int]
throw_many_times 0 = return []
throw_many_times n = do 
    r <- throwDice 
    rs <- throw_many_times (n-1)
    return (r:rs)

freq_of_point :: [Int] -> Int -> Float
freq_of_point ps point = fromIntegral k /fromIntegral n
    where
        k = length [x | x <- ps , x == point]
        n = length ps
        
dice_freq :: Int -> Int -> IO Float
dice_freq point m = do 
    ds <- throw_many_times m
    let r = freq_of_point ds point
    return r

dice_point_frequencies :: Int -> Int -> IO [Float]
dice_point_frequencies 0 _= return []
dice_point_frequencies n t= do
    x <- dice_freq t 100
    xs <- dice_point_frequencies (n-1) t
    return (x:xs)

average :: [Float] -> Float
average xs = sum xs / fromIntegral (length xs)

dice_point_pro :: Int -> Int -> IO Float
dice_point_pro n t = do 
    xs <- dice_point_frequencies n t
    return (average xs)

rep :: IO a -> Int -> IO [a] 
rep _ 0 = return []
rep action n= do
    x <- action
    xs <- rep action (n-1)
    return (x:xs)

throw6Dice :: IO [Int]
throw6Dice = rep throwDice 6

frequency :: [a] -> (a -> Bool) -> Float
frequency xs ieEvent = fromIntegral (length [x | x <- xs ,ieEvent x])
                     / fromIntegral (length xs)

atleast_on_point_fre :: Int -> IO Float
atleast_on_point_fre n = do
    xs <- rep throw6Dice n
    return (frequency xs (\x -> elem 6 x))

randomPoint :: IO (Float,Float)
randomPoint = do 
    x <- randomRIO (0,2.0)
    y <- randomRIO (0,2.0)
    return (x,y)

inCircleFre :: IO Float
inCircleFre = do
    let n = 1000
    xs <- rep randomPoint n
    let y = length [x | x <- xs ,inCircle x]
    return (fromIntegral y / fromIntegral n)

inCircle :: (Float, Float) -> Bool
inCircle (x,y) = sqrt ((abs (x - 1.0))**2+(abs (y - 1.0))**2 ) <= 1

compute_pi :: IO Float
compute_pi = do 
    let m = 1000
    xs <- rep inCircleFre m
    let z = average xs
    return (4*z)