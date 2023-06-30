module Coin(coingame) where
    
import System.Random ( randomIO, Random(randomR, random), RandomGen )
import Lib(rep)

data Coin = Fron | Reve deriving(Eq,Show,Enum)

instance Random Coin where
    random :: RandomGen g => g -> (Coin, g)
    random g = case randomR (0,1) g of
        (r,g') -> (toEnum r :: Coin,g')
    randomR :: RandomGen g =>(Coin, Coin) -> g -> (Coin, g)
    randomR (a,b) g = case randomR (fromEnum a,fromEnum b) g of 
        (r,g') -> (toEnum r :: Coin,g')

coingame :: IO ()
coingame = do 
    r <- rep throwCoin 1000
    print r

throwCoin :: IO Coin
throwCoin = randomIO :: IO Coin