module Main where

import System.Environment
import Data.Char
import Data.Maybe
import Data.Fixed
import Control.Monad
import Control.Applicative
import Text.Printf

-- | Parsing module
-- | definition
newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(res, rest)]   -> error "Parser did not consume entire stream."
    _           -> error "Parser error."

item :: Parser Char
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c, cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (\cs -> [])

option :: Parser a -> Parser a -> Parser a
option  p q = Parser $ \s ->
  case parse p s of
    []     -> parse q s
    res    -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
  then unit c
  else failure

-- | Combinator

-- | optionally parse
optional :: Parser a -> Parser (Maybe a)
optional p = (return Nothing) <|> (liftM Just p)

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs) }

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a }

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close = do {_ <- open; x <- p; _ <- close; return x}

digit :: Parser Char
digit = satisfy isDigit

numberStr :: Parser String
numberStr = do
  s <- unarySign
  digits <- some digit
  return $ s ++ digits

number :: Parser Int
number = do
  nStr <- numberStr
  return $ read nStr

unarySign :: Parser String
unarySign = do
        signs <- many $ oneOf "+-"
        return $ if length(filter(=='-')(signs)) `mod` 2 == 0 then "" else "-"

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n

-- | Parse Double

zeroStr :: Parser String
zeroStr = string "0"

-- | [1-9]
nonZeroDigit :: Parser Char
nonZeroDigit = satisfy $ \c -> isDigit c && c /= '0'

-- | integer part as string
intPartStr :: Parser String
intPartStr = zeroStr <|> numberStr

-- | fractional part as string
fractionalPartStr :: Parser String
fractionalPartStr = do
    dot <- char '.'
    digits <- some digit
    return $ [dot] ++ digits

-- | exponent part as string
exponentPartStr :: Parser String
exponentPartStr = do
    e <- char 'e' <|> char 'E'
    sign <- char '+' <|> char '-' <|> return '+'
    digits <- some digit
    return $ [e] ++ [sign] ++ digits

real :: Parser Double
real = do
    sign <- unarySign
    intPart <- intPartStr
    fractionalPart <- fractionalPartStr <|> return ""
    exponentPart <- exponentPartStr <|> return ""
    return $ read (sign ++ intPart ++ fractionalPart ++ exponentPart)

-- | Calculator parser

-- | expr = term { addOp term }.
-- | term = factor { mulOp factor }.
-- | factor = primary | "-" primary | "+" primary
-- | primary = "(" expr ")" | double | sqrtOp
-- | sqrtOp = "v(" expr ")"
-- | addOp = "+" | "-".
-- | mulOp = "*".
-- | divOp = "/"
-- | modulusOp = "%"
-- | powerOp = "$"

data Expr
  = Add     Expr Expr
  | Mul     Expr Expr
  | Div     Expr Expr
  | Mod     Expr Expr
  | Pow     Expr Expr
  | Sub     Expr Expr
  | Sqrt    Expr
  | Neg     Expr
  | Lit     Double
  deriving Show

-- | eval AST
eval :: Expr -> Double
eval ex = case ex of
  Add   a b -> eval a + eval b
  Mul   a b -> eval a * eval b
  Div   a b -> eval a / eval b
  Mod   a b -> eval a `mod'` eval b
  Pow   a b -> eval a ** eval b
  Sub   a b -> eval a - eval b
  Sqrt  a   -> sqrt $ eval a
  Lit   n   -> n
  Neg   n   -> - eval n

double :: Parser Expr
double = do
  n <- real
  return (Lit n)

applyUnarySign :: Parser Expr -> Parser Expr
applyUnarySign pe = do
        sign <- unarySign
        e <- pe
        return $ if sign == "-" then Neg e else e


sqrtOp :: Parser Expr
sqrtOp = do
    reserved "v"
    n <- parens expr
    return $ Sqrt n

exprBetweenSpaces :: Parser Expr
exprBetweenSpaces = between spaces expr spaces

expr :: Parser Expr
expr = term `chainl1` addOp

term :: Parser Expr
term = factor `chainl1` mulOp

factor :: Parser Expr
factor = between spaces primary spaces

primary :: Parser Expr
primary =
            double
        <|> applyUnarySign(sqrtOp <|> parens exprBetweenSpaces)

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = do
    reserved x
    return f

addOp :: Parser (Expr -> Expr -> Expr)
addOp = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = (infixOp "*" Mul) <|> (infixOp "/" Div) <|> (infixOp "%" Mod) <|> (infixOp "^" Pow)

run :: String -> Expr
run = runParser exprBetweenSpaces

main :: IO ()
main = do
        args <- getArgs
        printf "%.2f\n" $ eval $ run $ head args
