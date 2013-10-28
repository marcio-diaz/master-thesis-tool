module Main(main) where

import System.Environment(getProgName)

import Data.List(intersperse)

--import Prepo.HyLoAST(HyLoAST, astToString)
import Prepo.AST -- added
import Prepo.HyLoAST -- added
--import Translations(TargetFOL, layeredTranslation)
import OFTranslation -- added
import ModalSpassWriter
import ModalTranslator2(translator)

main :: IO ()
main = translator id writerBuilder

writerBuilder :: [HyLoAST] -> IO ([HyLoAST] -> String)
writerBuilder fs =
    do
        prgName <- getProgName
        let header = Problem{problemId = "modalInSpassSintax",
                             name        = "Modal problem in SPASS sintax",
                             author      = prgName,
                             logic       = Just "K",
                             status      = Unknown,
                             description = "Vacia"}
        return $ spassFormat header
