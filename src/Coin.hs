module Coin() where
import System.Random(randomIO)
import Lib(rep)

data Coin = Fron | Reve deriving(Eq,Show)

instance Random Coin where
-- 在这里写代码

coingame = print (rep throwCoin 1000)

throwCoin :: IO Coin
throwCoin = randomIO :: IO Coin
