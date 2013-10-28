module Main(main) where

import OFTranslation
import STptpWriter
import OFGenericTranslator

main :: IO ()
main = translator pnfFuncTransUnsorted (const . return $ tptpFormat)
