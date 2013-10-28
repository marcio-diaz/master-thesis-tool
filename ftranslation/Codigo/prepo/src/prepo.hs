module Main(main) where

import System.IO
import System.Environment
import Control.Monad
import Data.Maybe(isNothing, fromJust)

import Prepo.HyLoASTBuilder
import Prepo.RewriteRulesParser

import Prepo.HyLoAST
import Prepo.ASTRewrite

data Handles = Handles{input :: Handle,
                       output :: Handle}

main :: IO ()
main =
    do
        args <- parseArgs
        unless (isNothing args) $ do
            let (rulesFile, handles) = fromJust args
            formulasF <- hGetContents (input handles)
            let asts = hyloAstBuilder . hyloLexer $ formulasF
            rewriteRules <- rewriteRulesFromFile rulesFile
            hPutStrLn stderr ("# of rules: " ++ (show . length $ rewriteRules))
            let rewritenAsts = map (normalForm rewriteRules) asts
            printAsts (output handles) rewritenAsts
            hClose $ output handles

parseArgs :: IO (Maybe (FilePath, Handles))
parseArgs =
    do
        args <- getArgs
        let cantArgs = length args
        if not $ cantArgs `elem` [1..3]
            then do
                     printUsage
                     return Nothing
            else do  
                    let rulesFile = args !! 0
                    let minArgs = Handles{input  = stdin,
                                          output = stdout}
                    if cantArgs == 1 
                        then return $ Just (rulesFile, minArgs)
                        else do
                                 inputH <- openFile (args !! 1) ReadMode
                                 if cantArgs == 2
                                     then return $ Just (rulesFile, minArgs{input = inputH})
                                     else do
                                              outputH <- openFile (args !! 2) WriteMode
                                              return $ Just (rulesFile, minArgs{input  = inputH,
                                                                        output = outputH})

printUsage :: IO ()
printUsage =
    do
        prgName <- getProgName
        putStr "Usage: "
        putStr prgName
        putStrLn " <rewriteRulesFile> [<inputFile> [<outputFile>]]"

printAsts :: Handle -> [HyLoAST] -> IO ()
printAsts h (first:rest) =
    do
        hPutStrLn h "begin"
        (hPutStr h) . astToString $ first
        mapM_ (\s -> do{hPutStrLn h ";"; (hPutStr h) . astToString $ s}) rest
        hPutStrLn h "\nend"
