module Main(main) where

import OFTranslation
import TptpWriter
import OFGenericTranslator

main :: IO ()
main = translator withoutSorts (const . return $ tptpFormat)
