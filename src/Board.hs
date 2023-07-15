{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Board(printBoard) where

-- 这个功能写的还可以，可以自定义使用的棋盘大小，我估计井字棋也可以用，如果要重写井字棋就用这个做棋盘。
-- 不过实际上并没有打印，是否把printBoard改成putBoard之类的比较好呢？算了，懒得改了

-- 输出棋盘，因为用了Show a约束，所以只要是Show类族的类型都可以接受。
printBoard :: Show a => Int -> Int -> [[a]] -> String
-- 最后一行，封底
printBoard width 0     _            = printWidth width "+" (take width $ repeat "-----") 
-- 没到最后一行，输出一行隔断，一行数据。
printBoard width hight (line:board) = printWidth width "+" (take width $ repeat "-----") 
    ++ printWidth width "|" line 
    ++ printBoard width (hight - 1) board

-- 递归的输出每个元素。
printWidth :: Show a => Int -> String -> [a] -> String
printWidth 0 s _        = s ++ "\n"
printWidth w s (l:line) = s ++ putIn l ++ printWidth (w-1) s line

-- 将元素居中。
putIn :: Show a => a -> String
putIn l = putSpace space ++ ll ++putSpace (5 - wid - space)
    where 
        -- 为了保证字符串和其他元素有相同的输出，将引号 "" 用filter去除
        -- 还应该考虑到ll长于5的情况，用take 切一刀。
        ll = take 5 $ filter (\x -> x /= '\"') $ show l
        wid = length ll
        space = (5 - wid) `div` 2

-- 输出空格的辅助函数，要几个给几个。
putSpace :: Int -> String
putSpace 0     = ""
putSpace space = " " ++ putSpace (space - 1)