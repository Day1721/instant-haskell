{-# LANGUAGE LambdaCase, GADTSyntax #-}

module LLVMTranslator (translate) where

import Ast
import TranslatorBase
import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.State

data TranslatorState = TranslatorState {
    vars :: M.Map String ValueType,  -- map identifier -> (register number | const)
    counter :: Int                   -- last used register
}

data ValueType where
    Register :: Int -> ValueType
    Constant :: Int -> ValueType

asReg :: ValueType -> Int
asReg (Register i) = i
asReg (Constant i) = 0

instance Show (ValueType) where
    show (Register i) = "%_" ++ show i
    show (Constant i) = show i

instance Semigroup TranslatorState where
    (TranslatorState v c) <> (TranslatorState v' c') = TranslatorState (v <> v') (max c c')

instance Monoid TranslatorState where
    mempty = TranslatorState mempty 0

updateVariable :: String -> ValueType -> StateT TranslatorState Runner ()
updateVariable v r = modify (\(TranslatorState vars counter) -> TranslatorState (newVars vars) $ maxx counter) where
    newVars = M.insert v r
    maxx = max $ asReg r

newRegister :: StateT TranslatorState Runner ValueType
newRegister = get >>= \s -> 
    modify modifier >> 
    return (Register $ counter s) where
        modifier :: TranslatorState -> TranslatorState
        modifier (TranslatorState vars counter) = TranslatorState vars $ counter + 1

translate :: Program -> Either String String
translate p = let 
    writer = runExceptT (translateT p) >>= 
        return . either Just (const Nothing)
    in case runWriter writer of
        (Just s, _) -> Left s
        (Nothing, s) -> Right s

translateT :: Program -> Runner ()
translateT p = tellHead >>
    translateProgram p >>
    tellTail

tellHead :: Runner ()
tellHead = 
    tell "declare i32 @printf(i8*, ...)\n" >>
    tell "@formatString = private constant [4 x i8] c\"%d\\0A\\00\"\n" >>
    tell "define i32 @main(){\n"

tellTail :: Runner ()
tellTail = 
    tellInstr "ret i32 0" >>
    tell "}\n"

translateProgram :: Program -> Runner ()
translateProgram p = evalStateT (foreach translateStatement p) mempty

instance Show Oper where
    show OPlus = "add"
    show OMinus = "sub"
    show OMulti = "mul"
    show ODiv = "sdiv"

translateStatement :: Statement -> StateT TranslatorState Runner ()
translateStatement = let
    expr :: Expr -> StateT TranslatorState Runner ValueType
    expr (EId x) = get >>= \s -> case M.lookup x $ vars s of
        Just i -> return i
        Nothing -> fail $ "Undeclared variable: " ++ x
    expr (ENum n) = return $ Constant n
    expr (EOper o l r) = expr l >>= \lv ->
        expr r >>= \rv -> 
        newRegister >>= \res ->
        tellInstr (show res ++ " = " ++ show o ++ " i32 " ++ show lv ++ ", " ++ show rv) >>
        return res

    print :: ValueType -> StateT TranslatorState Runner ()
    print i = tellInstr $ "call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @formatString, i32 0, i32 0), i32 " ++ show i ++ ")"
    in \case
        SAss x e -> expr e >>= \vr -> updateVariable x vr
        SExpr e -> expr e >>= print
