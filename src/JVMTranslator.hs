module JVMTranslator (translate) where

import Ast
import Data.Either (either)
import Control.Monad.Except
import Control.Monad.Writer

type RunnerMonad a = ExceptT String (Writer String) a

translate :: Program -> Either String String
translate p = let 
    writer = runExceptT (translateT p) >>= 
        return . either Just (const Nothing)
    in case runWriter writer of
        (Just s, _) -> Left s
        (Nothing, s) -> Right s

translateT :: Program -> RunnerMonad ()
translateT p = optimizeStack p >>= \p' ->
    countVariables p' >>
    foldMI translateStatement p'

optimizeStack :: Program -> RunnerMonad Program
optimizeStack = undefined

countVariables :: Program -> RunnerMonad ()
countVariables = undefined

translateStatement :: Statement -> RunnerMonad ()
translateStatement = undefined

foldMI :: (Monad m, Foldable f) => (a -> m ()) -> f a -> m ()
foldMI f c = foldM_ (const f) () c