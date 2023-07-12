{-# LANGUAGE ForeignFunctionInterface #-}
module Csin(cSin) where

foreign import ccall "c_sin" cSin :: Double -> Double