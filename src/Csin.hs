{-# LANGUAGE ForeignFunctionInterface #-}
module Csin(cSin, getsin) where

foreign import ccall "c_sin" cSin :: Double -> Double

getsin :: IO ()
getsin = do
    putStrLn "请输入一个Double值"
    n <- getLine
    print $ sin (read n::Double)