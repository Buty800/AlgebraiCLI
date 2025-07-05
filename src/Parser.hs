
-- Functional parsing library from chapter 13 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.
module Parsing (module Parsing, module Control.Applicative) where

import Control.Applicative
import Data.Char

import Types

-- Basic definitions
newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Sequencing parsers
instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\inp -> [(v,inp)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

-- Making choices
instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\inp -> [])
   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\inp -> case parse p inp of
                           []        -> parse q inp
                           [(v,out)] -> [(v,out)])

-- Derived primitives
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = char x >> string xs >> return (x:xs)

ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = some digit >>= return.read

int :: Parser Int
int = (char '-' >> nat >>= return.negate) <|> nat

float :: Parser Double
float = do 
    hole <- int 
    char '.' 
    dec <- some digit 
    return.read $ show hole ++ "." ++ dec
    <|> 
    (int >>= return.fromIntegral)

var :: Parser (Char,Int)
var =
  do 
  x <- letter
  char '_'
  n <- digit 
  return (x,read ['0',n])
  <|>
  do
  x <- letter 
  return (x,-1)

--Left asociative operators
(<|) :: Parser a -> Parser (a->a->a) -> Parser a
p <| op = p >>= rest 
    where
        rest x = do 
            f <- op
            y <- p
            rest $ f x y
            <|> return x

between :: String -> Parser a -> String -> Parser a
between open p close = do string open
                          x <- p 
                          string close
                          return x

expr = factor <| expop <| mulop <| addop
factor = 
  between "(" expr ")" <|> 
  (float >>= return.Constant) <|> 
  (var >>= return.Var) <|>
  (char '-' >> factor >>= return.(Sub (Constant 0)))

addop = (char '+' >> return Add) <|> (char '-' >> return Sub)
mulop = (char '*' >> return Mul) <|> (char '/' >> return Div) <|> (return Mul)
expop = char '^' >> return Pow

instance Read Expr where 
  readsPrec _ = parse expr 

