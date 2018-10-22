module Main (main) where

import MainBase (mainBase, TranslatorModule(..))
import LLVMTranslator

main :: IO ()
main = mainBase translator where
    translator = TranslatorModule (\s -> s ++ ".ll") translate
    