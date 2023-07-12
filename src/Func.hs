{-# LANGUAGE ParallelListComp #-}
module Func (foldn,add,mul,pow,squ,pow',fibs,fibs',primes) where

foldn :: Float -> (Float -> Float) -> Float -> Float
foldn z _ 0 = z
foldn z f n = f (foldn z f (n-1))

add :: Float -> Float -> Float
add m = foldn m (\x -> x + 1)

mul :: Float -> Float -> Float 
mul m = foldn 0 (add m) 

pow :: Float -> Float -> Float 
pow m = foldn 1 (mul m)

squ :: Float -> Float
squ n = foldn 0 (\x -> (sqrt x + 1)**2) n

pow' :: Float -> Float -> Float
pow' m = foldn 0 ((\m' n -> n ** m') m) 

fibs :: [Integer]
fibs = 0:1:[x + y | (x,y) <- zip fibs (tail fibs)]
-- 也可以用zipWith来代替
-- fibs = 0:1:[x| x <- zipWith (+) fibs (tail fibs)]

-- 通过paralle扩展实现并行式的列表生成式，我感觉这是一个很重要而且好用的特性，应该经常使用啊
-- 注意一点，如果两个列表的长度不相同，那么最终生成的列表也只和较短的一个等长。当然在实现无穷列表的时候就无所谓了。
fibs' :: [Integer]
fibs' = 0:1:[x+y|x<-fibs'|y<-(tail fibs')]

-- 艾拉托斯尼特筛法取素数
primes :: [Integer]
primes = sieve [2..]

-- 筛法
sieve :: [Integer] -> [Integer]
--  实际上没必要写空列表的情况，不过编译器会警告。实际上由于只有primes调用它，所以根本不会有空列表传入。
sieve [] = []
sieve (x:xs) = x : sieve [y|y <- xs,y `mod` x > 0]