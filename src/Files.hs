module Files (main_file,main_file2,read_write) where

import Data.Char (toLower)
import Data.List (sort,sortBy,group)
import Prelude hiding (lines) -- 还能通过hiding来隐藏

read_write :: IO ()
read_write = do
    ts <- readFile "file/datafile.txt"
    let rs = words ts 
    let n  = length rs
    writeFile "file/result.txt" ("Total number of words:" ++ show n)

main_file :: IO ()
main_file =  do
    ss <- readFile "file/input_data.txt"
    let ds = map (\x -> read x::Float) (words ss)
    print ds 

step1 :: String -> [String]
step1 lines = map (map toLower) (words lines)

step2 :: [String] -> [String]
step2 = sort

step3 :: [String] -> [(String,Int)]
step3 ss = map (\x -> (head x, length x)) (group ss)

step4 :: [(String,Int)] -> [(String,Int)]
step4 xs = sortBy comp xs

comp :: (String,Int) -> (String,Int) -> Ordering
comp (s,m) (t,n)
    | m < n = GT
    | m > n = LT
    | otherwise = compare s t

format :: [(String,Int)] -> String
format xs = unwords $ map (\(w,n) -> w ++ ":" ++ show n ++ "\n") xs 

main_file2 :: IO ()
main_file2 = do 
    lines <- readFile "file/wonderfulWorld.txt"
    let results = cout_fre lines
    writeFile "file/wonderfulWorld_word_fre.txt" (format results)

cout_fre :: String -> [(String,Int)]
cout_fre lines = step4$step3$step2$step1 lines