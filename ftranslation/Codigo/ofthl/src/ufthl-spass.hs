module Main(main) where

import System.Environment(getProgName)

import Data.List(intersperse)

--import Prepo.HyLoAST(HyLoAST, astToString)
import Prepo.AST -- added
import Prepo.HyLoAST -- added
--import Translations(TargetFOL, layeredTranslation)
import OFTranslation -- added
import FOLSpassWriter
import OFGenericTranslator(translator)

main :: IO ()
main = translator unsortedFuncTrans writerBuilder
    
  
writerBuilder :: [HyLoAST] -> IO ([FOL] -> String)
writerBuilder fs =
    do
        prgName <- getProgName
        let header = Problem{problemId = "functionalTranslated",
                             name        = "Optimized Functional Translation for H(@,!)",
                             author      = prgName,
                             logic       = Just "H(@,!)",
                             status      = Unknown,
                             description = "Vacia"}
--                             description = concat . ("Original formulas:\n    ":) .
--                                                     intersperse "    " . map ((++ "\n") . astToString) $ fs}
--        putStrLn "\n\nDebugging...\n"             
--        putStrLn "Traduccion Funcional:\n"             
--        putStrLn (concat (map ((++ "\n\n") . prettyPrintSFOL . functionalTranslation) fs))
--        putStrLn "Traduccion Funcional +  Prenex Normal Form:\n"             
--        putStrLn (concat (map ((++ "\n\n") . prettyPrintSFOL . pnfFuncTrans) fs))
--        putStrLn "Traduccion Funcional +  Prenex Normal Form + Mover los Existenciales de sort l + Remover sorts:\n"             
--        putStrLn (concat (map ((++ "\n\n") . prettyPrintFOL . optFuncTrans) fs))        

--        putStrLn "End debugging\n\n"             
        return $ spassFormat header
