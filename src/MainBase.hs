module MainBase (mainBase, TranslatorModule(TranslatorModule)) where

import System.Environment (getArgs)
import System.FilePath (dropExtension, takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Ast
import Analyser
import Parser

-- represents AST -> Code translation module
data TranslatorModule = TranslatorModule {
    sourceFileName :: String -> String,             -- filename without extension -> generated code filename 
    translate :: Program -> Either String String    -- translation function (by convention, returns Either Error Code)
}

mainBase :: TranslatorModule -> IO ()
mainBase translator = getArgs >>= mapM_ (parseFile translator)

parseFile :: TranslatorModule -> String -> IO ()
parseFile translator name = readFile name >>= \code -> let 
    res = runParsing name code >>= 
          asEither analyse >>= 
          translate translator
    targetFileName = sourceFileName translator $ dropExtension name
    in case res of
        Left err -> putStrLn err
        Right translatedCode -> let 
            dir = takeDirectory name
            in createDirectoryIfMissing True dir >> 
                writeFile name translatedCode

asEither :: (a -> Maybe b) -> a -> Either b a
asEither f x = case f x of
    Just r -> Left r
    Nothing -> Right x