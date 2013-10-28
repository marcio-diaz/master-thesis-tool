module Main(main) where

import System.Environment(getProgName)

import Data.List(intersperse)

--import Prepo.HyLoAST(HyLoAST, astToString)
import Prepo.AST -- added
import Prepo.HyLoAST -- added
--import Translations(TargetFOL, layeredTranslation)
import OFTranslation -- added
import SFOLSpassWriter
import FGenericTranslator(translator)

main :: IO ()
main = translator withSorts writerBuilder

writerBuilder :: [HyLoAST] -> IO ([SFOL] -> String)
writerBuilder fs =
    do
        prgName <- getProgName
        let header = Problem{problemId = "functionalTranslated",
                             name        = "Functional Translation for H(@,!)",
                             author      = prgName,
                             logic       = Just "H(@,!)",
                             status      = Unknown,
                             description = "Vacia"}
--                             description = concat . ("Original formulas:\n    ":) .
--                                                     intersperse "    " . map ((++ "\n") . astToString) $ fs}
        return $ spassFormat header
