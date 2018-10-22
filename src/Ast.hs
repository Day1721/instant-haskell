{-# LANGUAGE GADTSyntax, ExistentialQuantification #-}

module Ast where

type Ident = String

data Oper = OPlus | OMinus | OMulti | ODiv
    deriving (Show)

type Program = [Statement]

data Statement where
    SAss :: Ident -> Expr -> Statement
    SExpr :: Expr -> Statement
    deriving (Show)

data Expr where
    ENum :: Int -> Expr
    EId :: Ident -> Expr
    EOper :: Oper -> Expr -> Expr -> Expr
    deriving (Show)