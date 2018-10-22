module Main (main) where

import MainBase (mainBase, TranslatorModule(..))
import JVMTranslator

main :: IO ()
main = mainBase translator where
    translator = TranslatorModule (\s -> s ++ ".j") translate