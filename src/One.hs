{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-Wno-unused-top-binds #-}


-- 看起来haskell并不支持==在不同类型间使用，所以下面的做法从根上就行不通。
-- 有一个新的想法，我能不能自己写一个Eq class让他支持在不同类型见间比较呢？之后可以试试看
-- 下面是代码：


-- module One where

-- class OneUnit a 

-- data One = One

-- instance OneUnit One

-- instance OneUnit Bool

-- instance OneUnit Int

-- instance Eq One where
--     (==) :: forall a .OneUnit a => a -> a -> Bool
--     (==) One True = False
--     (==) One False = True
--     (==) True One= False
--     (==) False One= True
--     (==) One 1 = False
--     (==) One 0 =True
--     (==) 1 One= False
--     (==) 0 One= True
--     (/=) :: forall a .OneUnit a => a -> a -> Bool
--     (/=) One True = False
--     (/=) One False = True
--     (/=) True One= False
--     (/=) False One= True
--     (/=) One 1 = False
--     (/=) One 0 =True
--     (/=) 1 One= False
--     (/=) 0 One= True