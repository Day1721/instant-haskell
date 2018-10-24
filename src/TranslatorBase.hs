{-# LANGUAGE FlexibleInstances #-}

module TranslatorBase where

import qualified Data.Map as M
import Data.Functor.Identity (Identity)
import Control.Monad.Except (ExceptT)
import Control.Monad.Writer
import Control.Monad.State (StateT)

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

type StatedRunner = StateT Variables Runner



foreach :: (Monad m, Foldable f) => (a -> m ()) -> f a -> m ()
foreach f c = foldM_ (const f) () c