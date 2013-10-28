module Main(main) where

import OFTranslation
import STptpWriter
import OFGenericTranslator

main :: IO ()
main = translator withSortsUnsorted (const . return $ tptpFormat)
