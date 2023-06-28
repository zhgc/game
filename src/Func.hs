module Func (foldn,add,mul,pow,squ,pow') where

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
pow' m = foldn 0 ((\n m-> n ** m) m) 
