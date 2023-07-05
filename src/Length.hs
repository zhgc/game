{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
module Length () where

class LengthUnit a 

data Length a where
    Length :: LengthUnit a => Double -> Length a
    MyLength :: LengthUnit a => Int -> Length a 
    WhatLength :: LengthUnit a => [Int] -> Length a 
deriving stock instance Show a => Show (Length a)
-- 现在的问题是show实际上没有实现，所以下面的几个函数是打印不了的。
data Meter
instance LengthUnit Meter

data Foot
instance LengthUnit Foot

meterLength :: Length Meter 
meterLength = Length 3.0

meterMyLength :: Length Meter
meterMyLength = MyLength 3

meterWhatLength :: Length Meter
meterWhatLength = WhatLength [1,2,3,4,5,6]