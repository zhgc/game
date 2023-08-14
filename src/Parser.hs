{-# LANGUAGE LambdaCase #-}
module Parser where
import Data.Char (isSpace, isDigit)

data Ops = Add | Mul
data Expr = Num Int 
          | Bin Ops Expr Expr 
data Parser a  = Parser {runParser :: String -> [(a,String)]}

getc :: Parser Char
getc = Parser (\case
    []     -> []
    (x:xs) -> [(x,xs)])

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do 
    c <- getc
    if p c then return c else failed 

failed :: Parser a
failed = Parser (\_ -> [])

char :: Char -> Parser ()
char c = do { _ <- satisfy (==c);return ()}

string :: String -> Parser ()
string [] = return ()
string (x:xs) = do {char x; string xs; return ()}

(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p q = Parser f 
    where 
        -- 局部函数定义
        f s = let ps = runParser p s 
              in if null ps then runParser q s else ps 

none :: Parser [a]
none = return []

many :: Parser a -> Parser [a]
many p = do {x <- p;xs <- many p ;return (x:xs)} <|> none

space :: Parser ()
space = many (satisfy isSpace) >> return ()

-- 这句没看懂`
some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p 

symbol :: String -> Parser ()
symbol xs = space >> string xs 

natural :: Parser Int 
natural = do {ds <- some (satisfy isDigit); return (read ds::Int)}

number :: Parser Expr
number = do {space; n <- natural; return (Num n)}

binaryParser :: Parser Expr
binaryParser = do 
    e1 <- exprParser
    op <- opParser
    e2 <- exprParser
    return (Bin op e1 e2)

exprParser :: Parser Expr
exprParser = token (number <|> paren binaryParser)

-- 这个我自己实现的，
opParser :: Parser Ops 
opParser = (symbol "+" >> return Add) <|> (symbol "*" >> return Mul )

token :: Parser a -> Parser a
token p = space >> p

paren :: Parser a -> Parser a
paren p = do {symbol "(" ; e <- p ; symbol ")" ; return e}

getExpr::String -> Either String Expr
getExpr s
    | null ps   = Left "Error"
    | otherwise = Right $ fst $ head ps
    where 
        ps = runParser exprParser s

compute :: IO () 
compute = do 
    putStr "Input an expression:"
    s <- getLine
    putStr "Answer: "
    case getExpr s of 
        (Left  e) -> print e 
        (Right e) -> print $ eval e

-- 现在还有一个问题，如何让compute可以识别省略括号的形式

eval :: Expr -> Int
eval (Num n) = n
eval (Bin Add x y) = (eval x) + (eval y)
eval (Bin Mul x y) = (eval x) * (eval y)

instance Show Ops where
    show :: Ops -> String
    show Add = "+"
    show Mul = "*"

instance Show Expr where 
    show :: Expr -> String
    show (Num n)      = show n
    show (Bin op a b) = "(" ++ show a ++ show op ++ show b ++ ")"

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = Parser (\s -> [(f x,y) | (x,y) <- runParser p s])

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser (\s -> [(x,s)])

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) p q = Parser (\s -> [(f x,ys)|
                               (f,xs) <- runParser p s,
                               (x,ys) <- runParser q xs])

instance Monad Parser where
    return :: a -> Parser a
    return = pure

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) p f = Parser (\s -> [ys|
                               (x,xs) <- runParser p s,
                               ys     <- runParser (f x) xs])