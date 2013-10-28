module Main(main) where

import OFTranslation
import STptpWriter
import OFGenericTranslator

main :: IO ()
main = translator funcTransUnsorted (const . return $ tptpFormat)
