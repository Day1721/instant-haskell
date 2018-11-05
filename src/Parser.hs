module Parser (
    runParsing,
    testParser,
    ) where

import Ast
import Control.Monad
import Control.Applicative (empty)
import Control.Monad.Combinators.Expr
import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

runParsing :: String -> String -> Either String Program
runParsing name code = case runParser manyParser name code of
    Left err -> Left $ show err
    Right p -> Right p

testParser :: String -> IO ()
testParser = parseTest manyParser


sc :: Parser ()
sc = L.space space1 empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

decimal :: Integral a => Parser a
decimal = lexeme L.decimal

semicolon :: Parser String
semicolon = symbol ";"

identifier :: Parser String
identifier = (lexeme . try) $ (:) <$> letterChar <*> many alphaNumChar

expr :: Parser Expr
expr = makeExprParser terminals operators where
    terminals :: Parser Expr
    terminals = EId  <$> identifier
            <|> ENum <$> decimal
            <|> between (symbol "(") (symbol ")") expr

    operators :: [[Operator Parser Expr]]
    operators = [[ 
            InfixL (EOper OMulti <$ symbol "*"),
            InfixL (EOper ODiv   <$ symbol "/")
        ], [ 
            InfixL (EOper OMinus <$ symbol "-")
        ],[
            InfixR (EOper OPlus  <$ symbol "+")
        ]]

expressionP :: Parser Statement
expressionP = SExpr <$> expr

assignmentP :: Parser Statement
assignmentP = identifier >>= \i ->
    symbol "=" >>
    expressionP >>= \(SExpr e) ->
    return $ SAss i e

statementsP :: Parser [Statement]
statementsP = sepBy1 statementP semicolon

statementP :: Parser Statement
statementP = try assignmentP
         <|> expressionP

manyParser :: Parser [Statement]
manyParser = between sc eof statementsP