{-# LANGUAGE ParallelListComp #-}
module Func (foldn,add,mul,pow,squ,pow',fibs,fibs',primes,evens,odds,list4,list4',list5,list5',isPrime,isPrime',hamming,hamming') where
import Control.Monad (ap)
import GHC.Base (join)

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

evens :: [Int]
evens = filter (\x-> x`mod`2 == 0) [0..]

odds :: [Int]
odds = [x+1| x <- evens]

list4 :: [Int]
list4 = 0:concat [[x,-x]|x<-[1..]]

list4' :: [Int]
list4' = iterate (\x -> if x <= 0 then (x - 1) * (-1) else - x ) 0

list5 :: [Int]
list5 = [11..]

list5' :: [Int]
list5' = iterate (\x -> succ x) 11

-- 但这个被网友吹的挺高，怎么连0，1都被识别成素数啊，不会是因为primes的定义没有配套跟上吧。
isPrime :: Integer -> Bool
isPrime = ap (all.((0/=).).mod) $ flip takeWhile primes.(.join(*)).flip (<=) 

-- 不能这样写，因为primes是无穷序列，所以elem会一直找。
isPrime' :: Integer -> Bool
isPrime' x = x `elem` primes

hamming :: [Int]
hamming = hsieve [1..]

hsieve :: [Int] -> [Int]
hsieve [] = []
hsieve (x:xs) = x : [y | y<- xs ,isHamming y]

-- 可以了，这个成功了，不过速度真的很慢，慢的无法接受，应该继续想办法。
-- 现在修改了，性能勉强可以接受，不过还是很慢，应该继续想办法。
isHamming :: Int -> Bool
isHamming x = or [x == ((2^i)*(3^j)*(5^k))|i<-[0..x `div` 2],j<-[0..x `div` 3],k<-[0..x `div` 5]]

-- 我想到了bmp串匹配算法，获得从小到大的hamming数列不也需要ijk前后移动吗？也许这是一个突破口？



-- ok，这个版本的性能再次大幅提升，核心在于一个hamming数必然由hamming数组成，要从因子中去除非hamming的部分。
-- 不过问题在于，虽然大幅提升，但数量级达到三位数之后，速度还是逐渐变得很慢，性能提升仍需继续。
-- 继续优化，减少了ijk的重复运算，感觉像回到了大一优化鸡兔同笼一般。
-- 继续优化，不过还是小优化，这次优化的关键在于x的因子不会超过sqrt x
-- 上一条优化有问题
-- 跳出一个思维陷阱，那就是汉明数需要是三个数相乘，实际上不需要，汉明数只需要保证他的因数也是汉明数就可以了，所以删除k，性能真的大幅提升了。这次是真的可以用了，一百以上也很快。
hamming' :: [Int]
hamming' = 1:2:3:4:5:hs 5 5

hs :: Int -> Int -> [Int]
hs x n = m:hs m (n+1)
    where
        -- 
        xs = take n hamming'
        -- 删除k
        -- 又想到一点，j 不应该是大于i，而应该是大于x除i，而i不应该大于sqrtx
        ms = [i*j|i<-takei xs x,j<-dp xs (x `div` i),i*j>x]
        m = minimum ms 

takei :: [Int] -> Int->[Int]
takei xs n = take (geti xs n) xs

sqfl :: Int->Int
sqfl n = floor $ sqrt $ fromIntegral n

geti :: [Int] -> Int -> Int
geti []     _ = 0
geti (x:xs) n
    | sqfl n < x    = 1
    | otherwise = 1 + geti xs n

dp :: [Int] -> Int -> [Int]
dp xs i = drop (getL xs i - 1) xs

getL :: [Int] -> Int -> Int
getL []     _ = 0
getL (x:xs) n
    | n < x     = 1
    | otherwise = 1 + getL xs n

-- 突然想到一个超棒的优化策略。等我写下来。
-- 仔细一想不成立，算了。
-- hamming'' :: [Int]
-- hamming'' = 1:2:3:4:5:hs' 5 5

-- hs' :: Int -> Int -> [Int]
-- hs' x n = m:hs m (n+1)
--     where
