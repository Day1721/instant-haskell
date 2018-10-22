{-# LANGUAGE LambdaCase #-}

module Analyser (analyse) where

import Ast
import Data.Either (either)
import Control.Monad (foldM)

type State = [String] --defined variables

analyse :: Program -> Maybe String
analyse = either Just (const Nothing) . go where
    go :: Program -> Either String State
    go = foldM checkOne []

failure :: String -> Either String a
failure = Left

checkOne :: State -> Statement -> Either String State 
checkOne s = \case
    SAss x e -> checkOne s (SExpr e) >> return (x:s)
    SExpr e -> case e of
        EId x -> if elem x s then return s else failure $ "Non-assigned variable: " ++ x
        EOper _ e1 e2 -> 
            checkOne s (SExpr e1) >> 
            checkOne s (SExpr e2)
        _ -> return s