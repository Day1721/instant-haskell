{-# LANGUAGE LambdaCase, 
             DeriveFunctor, 
             FlexibleInstances #-}

module JVMTranslator (translate) where

import Ast
import Data.Either (either)
import Data.List (nub)
import Data.Char (toUpper)
import qualified Data.Map as M 
import System.FilePath (takeBaseName)
import Data.Functor.Identity (Identity)
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.State
import Debug.Trace

type RunnerT m = ExceptT String (WriterT String m)
type Runner = RunnerT Identity
type Variables = M.Map String Int

-- induction here: "tellInstr" can return any monad, which has "RunnerT" somewhere inside
class Translator m where
    tellInstr :: String -> m ()

instance {-# OVERLAPPING #-} Monad m => Translator (RunnerT m) where
    tellInstr s = tell $ "\t" ++ s ++ "\n"

-- added here just because wouldn't use UndecidableInstances
instance (Monad i, Translator i, MonadTrans m) => Translator (m i) where
    tellInstr = lift . tellInstr 

translate :: String -> Program -> Either String String
translate name p = let 
    writer = runExceptT (translateT name p) >>= 
        return . either Just (const Nothing)
    in case runWriter writer of
        (Just s, _) -> Left s
        (Nothing, s) -> Right s

translateT :: String -> Program -> Runner ()
translateT name p = tellHead name >>
    optimizeStack p >>= \p' ->
    countVariables p' >>
    translateProgram p' >>
    tellTail

tellLoad :: Translator m => Int -> m ()
tellLoad n | n <= 3 = tellInstr $ "iload_" ++ show n
           | otherwise = tellInstr $ "iload " ++ show n

tellStore :: Translator m => Int -> m ()
tellStore n | n <= 3 = tellInstr $ "istore_" ++ show n
            | otherwise = tellInstr $ "istore " ++ show n

tellConst :: Translator m => Int -> m ()
tellConst n | n > 0 && n <= 5 = tellInstr $ "iconst_" ++ show n
            | n == -1 = tellInstr $ "iconst_m1"
            | otherwise = tellInstr $ "bipush " ++ show n

tellHead :: String -> Runner ()
tellHead name = tell (".class public " ++ className ++ "\n") >>
    tell ".super java/lang/Object\n\n" >>
    tell ".method public static main([Ljava/lang/String;)V\n" where
        className = upperFirst $ takeBaseName name
        upperFirst (h:t) = toUpper h : t

tellTail :: Runner ()
tellTail = tellInstr "return" >>
    tell ".end method\n"

    
data DepthCounter a = DepthCounter {
    counterDepth :: Int,
    counterValue :: a
} deriving (Functor)

instance Semigroup a => Semigroup (DepthCounter a) where
    (DepthCounter d v) <> (DepthCounter d' v') = DepthCounter (max d d') (v <> v')

instance Monoid a => Monoid (DepthCounter a) where
    mempty = DepthCounter 0 mempty


optimizeStack :: Program -> Runner Program
optimizeStack p = let
    optimizeStatement :: Statement -> DepthCounter Statement
    optimizeStatement (SAss x e) = SAss x <$> optimizeExpr e
    optimizeStatement (SExpr e) = let 
        DepthCounter d e' = optimizeExpr e
        in DepthCounter (d+1) $ SExpr e'    -- +1 because of println at the end

    optimizeExpr :: Expr -> DepthCounter Expr
    optimizeExpr expr = let 
        go :: Expr -> (Oper -> Expr -> Expr -> Ordering -> Expr) -> DepthCounter Expr
        go (EOper o l r) mkExpr = let
            DepthCounter ld lv = optimizeExpr l
            DepthCounter rd rv = optimizeExpr r
            in case compare ld rd of
                LT -> DepthCounter rd       (mkExpr o lv rv LT)
                EQ -> DepthCounter (ld + 1) (mkExpr o lv rv EQ)
                GT -> DepthCounter ld       (mkExpr o lv rv GT)

        optimizeExact e = go e swipe where
            swipe o l r = \case
                LT -> EOper o r l
                _ -> EOper o l r

        -- optimizeExact (EOper o l r) = let
        --     DepthCounter ld lv = optimizeExpr l
        --     DepthCounter rd rv = optimizeExpr r
        --     in case trace (show $ compare ld rd) (compare ld rd) of
        --         LT -> DepthCounter rd       (EOper o rv lv)
        --         EQ -> DepthCounter (ld + 1) (EOper o lv rv)
        --         GT -> DepthCounter ld       (EOper o lv rv)

        justCount = flip go $ \o l r _ -> EOper o l r
        in case expr of
            EOper OPlus l r -> optimizeExact expr
            EOper OMinus l r -> justCount expr
            EOper OMulti l r -> optimizeExact expr
            EOper ODiv l r -> justCount expr
            _ -> DepthCounter 1 expr

    (DepthCounter depth stmts) = foldMap (fmap pure . optimizeStatement) p
    in tellInstr (".limit stack " ++ show depth) >>
    return stmts

countVariables :: Program -> Runner ()
countVariables = let 
    collector c = \case
        SAss v e -> v:c
        _ -> c
    amount = length . nub . foldl collector [] 
    limiter = tellInstr . (".limit locals " ++)
    in limiter . show . (+1) . amount

translateProgram :: Program -> Runner ()
translateProgram p = evalStateT (foreach translateStatement p) M.empty


type StatedRunner = StateT Variables Runner

translateStatement :: Statement -> StatedRunner ()
translateStatement = let 
    translateExpression :: Expr -> StatedRunner ()
    translateExpression = \case
        ENum n -> tellConst n
        EId x -> get >>= \s -> case M.lookup x s of
            Just idx -> tellLoad idx
            Nothing -> fail $ "Non-defined variable: " ++ x
        EOper oper left right -> translateExpression left >>
            translateExpression right >>
            tellInstr (operToInstr oper)
    
    operToInstr :: Oper -> String
    operToInstr OPlus = "iadd"
    operToInstr OMinus = "isub"
    operToInstr OMulti = "imul"
    operToInstr ODiv = "idiv"

    printResultOf :: StatedRunner () -> StatedRunner ()
    printResultOf innerMonad = tellInstr "getstatic java/lang/System/out Ljava/io/PrintStream;" >>
        innerMonad >>
        tellInstr "invokevirtual java/io/PrintStream/println(I)V" 
    
    assignTo :: StatedRunner () -> String -> StatedRunner ()
    assignTo expr x = expr >> 
        get >>= \s -> case M.lookup x s of
            Just idx -> tellStore idx
            Nothing -> modify (M.insert x maxx) >> 
                tellStore maxx where
                    maxx = 1 + maximum (M.elems s)
    
    in \case
        SExpr e -> printResultOf (translateExpression e)
        SAss x e -> assignTo (translateExpression e) x


foreach :: (Monad m, Foldable f) => (a -> m ()) -> f a -> m ()
foreach f c = foldM_ (const f) () c