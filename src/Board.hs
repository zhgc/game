{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Board(printBoard) where

-- 这个功能写的还可以，可以自定义使用的棋盘大小，我估计井字棋也可以用，如果要重写井字棋就用这个做棋盘。

printBoard :: Show a => Int -> Int -> [[a]] -> String
printBoard width 0     _            = printWidth width "+" (take width $ repeat "-----") 
printBoard width hight (line:board) = printWidth width "+" (take width $ repeat "-----") 
    ++ printWidth width "|" line 
    ++ printBoard width (hight - 1) board

printWidth :: Show a => Int -> String -> [a] -> String
printWidth 0 s _        = s ++ "\n"
printWidth w s (l:line) = s ++ putIn l ++ printWidth (w-1) s line

putIn :: Show a => a -> String
putIn l = putSpace space ++ ll ++putSpace (5 - wid - space)
    where 
        -- 为了保证字符串和其他元素有相同的输出，将引号 "" 用filter去除
        -- 还应该考虑到ll长于5的情况，用take 切一刀。
        ll = take 5 $ filter (\x -> x /= '\"') $ show l
        wid = length ll
        space = (5 - wid) `div` 2

putSpace :: Int -> String
putSpace 0     = ""
putSpace space = " " ++ putSpace (space - 1)