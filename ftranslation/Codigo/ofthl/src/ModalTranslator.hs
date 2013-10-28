module ModalTranslator(translator) where

import System.IO
import System.Environment(getArgs, getProgName)
import Control.Monad(unless)
import Data.Maybe(isNothing, fromJust)

import Prepo.HyLoASTBuilder(hyloAstBuilder, hyloLexer)
import Prepo.HyLoAST
import Prepo.AST
import OFTranslation

data Handles = Handles{input :: Handle,
                       output :: Handle}

translator :: (HyLoAST -> HyLoAST) -> ([HyLoAST] -> IO ([HyLoAST]-> String)) -> IO ()
translator translation writerBuilder =
    do
        args <- parseArgs
        unless (isNothing args) $ do
            let handles = fromJust args
            formulasF <- hGetContents (input handles)
            let asts = [joinForms (hyloAstBuilder . hyloLexer $ formulasF)]
            writer <- writerBuilder asts
            hPutStr (output handles) . writer . map translation $ asts
            hClose $ output handles

parseArgs :: IO (Maybe Handles)
parseArgs =
    do
        args <- getArgs
        let cantArgs = length args
        case cantArgs of
            0 -> return $ Just Handles{input  = stdin,
                                       output = stdout}
            1 -> do
                    inputH <- openFile (args !! 0) ReadMode
                    return $ Just Handles{input  = inputH,
                                          output = stdout}
            2 -> do
                    inputH  <- openFile (args !! 0) ReadMode
                    outputH <- openFile (args !! 1) WriteMode
                    return $ Just Handles{input  = inputH,
                                          output = outputH}
            _ -> printUsage >> return Nothing

printUsage :: IO ()
printUsage =
    do
        prgName <- getProgName
        putStr "Usage: "
        putStr prgName
        putStrLn "[<inputFile> [<outputFile>]]"
