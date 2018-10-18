{-# LANGUAGE GADTSyntax, ExistentialQuantification #-}

module Parser (runParsing) where

import Control.Monad
import Control.Applicative (empty)
import Control.Monad.Combinators.Expr
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type Ident = String

data Oper = OPlus | OMinus | OMulti | ODiv

applyOper :: Integral a => Oper -> a -> a -> a 
applyOper OPlus = (+)
applyOper OMinus = (-)
applyOper OMulti = (*)
applyOper ODiv = quot

newtype Program = Program [Statement]

data Statement where
    SAss :: Ident -> Expr -> Statement
    SExpr :: Expr -> Statement

data Expr where
    ENum :: Integral a => a -> Expr
    EId :: Ident -> Expr
    EOper :: Oper -> Expr -> Expr -> Expr

runParsing :: String -> Program
runParsing = undefined

sc :: Parser ()
sc = L.space space1 empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

--parseStatement :: String -> Statement

delim :: Parser String
delim = symbol ";"

variable :: Parser String
variable = (lexeme . try) $ (:) <$> letterChar <*> many alphaNumChar

expr :: Parser Expr
expr = makeExprParser terminals operators where
    terminals :: Parser Expr
    terminals = undefined

    operators :: [[Operator Parser Expr]]
    operators = [[ 
            InfixL (EOper OMulti <$ symbol "*"), 
            InfixL (EOper ODiv   <$ symbol "/") 
        ], [ 
            InfixR (EOper OPlus  <$ symbol "+"), 
            InfixL (EOper OMinus <$ symbol "-") 
        ]]