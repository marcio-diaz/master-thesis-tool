#! /usr/bin/env runghc

import HyLo.Signature.Simple (NomSymbol(..), PropSymbol(..), RelSymbol(..))

import qualified HyLo.InputFile as InputFile
import qualified HyLo.Formula as F

import Data.List ( unfoldr )

import System.Cmd (system)
import System.Exit ( ExitCode (..) )
import System.Environment ( getArgs, getProgName )
import System.Directory ( doesFileExist , getPermissions, Permissions (..) )

import Control.Applicative ( (<$>) )
import Control.Monad ( when )

main :: IO ()
main =
 do args <- getArgs
    if length args /= 6
     then showHelp
     else do let [nbP,nbN,nbR,startAt] = map read $ take 4 args
             let [cmd1,cmd2] = drop 4 args
             let prover1Path = takeWhile (/= ' ') cmd1
             let prover2Path = takeWhile (/= ' ') cmd2
             prover1Exec <- isExec prover1Path
             prover2Exec <- isExec prover2Path
             if not $ and [prover1Exec , prover2Exec]
              then showHelp
              else mainLoop cmd1 cmd2 startAt ( drop startAt $ allFsOneList (getProps nbP) (getNoms nbN) (getRels nbR) )

mainLoop :: (Show n , Show p, Show r) => String -> String -> Int -> [F.Formula n p r] -> IO ()
mainLoop cmd1 cmd2 step (hd:tl) =
 do writeFormulaInFile hd step
    result <- execOnCurrentFile cmd1 cmd2 step
    if result
     then mainLoop cmd1 cmd2 (step+1) tl
     else putStrLn $ "We have a problem : " ++ show hd


isExec :: FilePath -> IO Bool
isExec file = do exists <- doesFileExist file
                 if exists then executable <$> getPermissions file
                           else return False

showHelp :: IO ()
showHelp =
 do name <- getProgName
    putStrLn $ unlines
     ["Usage : " ++ name ++ " nb_props nb_noms nb_rels start_at_Nth_formula_(0=1st) \"prover1\" \"prover2\""]


getProps :: Int -> [PropSymbol]
getProps n = map PropSymbol [0..(n-1)]

getNoms :: Int -> [NomSymbol]
getNoms n = map N [0..(n-1)]

getRels :: Int -> [RelSymbol]
getRels n = map RelSymbol [0..(n-1)]

writeFormulaInFile :: (Show n , Show p, Show r) => F.Formula n p r  -> Int -> IO ()
writeFormulaInFile f step = writeFile "input" (unlines ["%" ++ show step,
                                                        "signature { automatic } theory {",
                                                        show f, "}"])

execOnCurrentFile :: String -> String -> Int -> IO Bool
execOnCurrentFile cmd1 cmd2 step =
 do prover1_code <- runProver cmd1
    prover2_code <- runProver cmd2
    (mBool,msg)  <- checkMatch prover1_code prover2_code
    when (step `mod` 100 == 0) (putStrLn $ "[" ++ show step ++ "] " ++ msg)
    case mBool of
     Just False -> return False
     _          -> return True
     
checkMatch :: ExitCode -> ExitCode -> IO (Maybe Bool,String)
checkMatch code1 code2 =
 case (code1,code2) of
   (ExitFailure 3,      _      ) -> return (Nothing, "1 timeout")
   (      _      ,ExitFailure 3) -> return (Nothing, "2 timeout")
   (ExitFailure 1,ExitFailure 1) -> return (Just True, "OK (sat)")
   (ExitFailure 2,ExitFailure 2) -> return (Just True, "OK (unsat)")
   (ExitFailure 1,ExitFailure 2) -> return (Just False, "Fail (1 sat, 2 unsat)")
   (ExitFailure 2,ExitFailure 1) -> return (Just False, "Fail (1 unsat, 2 sat)")
   _                             -> return (Nothing, "Something weird happened")

runProver :: String -> IO ExitCode
runProver cmd = system $ cmd ++ defaultExtraParams

defaultExtraParams :: String
defaultExtraParams = " -t 5 -q -f input "

--
-- "exhaustive" list of formulas for a given signature
--

allFsOneList :: [PropSymbol] -> [NomSymbol] -> [RelSymbol] -> [F.Formula NomSymbol PropSymbol RelSymbol]
allFsOneList ps ns rs = concat $ allFs ps ns rs

allFs :: [PropSymbol] -> [NomSymbol] -> [RelSymbol] -> [[F.Formula NomSymbol PropSymbol RelSymbol]]
allFs ps ns rs =
   ( [F.Top, F.Bot] ++ (fsSizeOneNoTrivial ps ns rs) )
  :(unfoldr (doStepPlusOne ps ns rs) 1)

doStepPlusOne :: [PropSymbol] -> [NomSymbol] -> [RelSymbol] -> Int -> Maybe ([F.Formula NomSymbol PropSymbol RelSymbol],Int)
doStepPlusOne ps ns rs s =
  Just (
      cartProdApp F.Diam  rs   ((allFs ps ns rs) !! (s - 1))
   ++ cartProdApp F.At    ns   ((allFs ps ns rs) !! (s - 1))
   ++ map         F.E     ((allFs ps ns rs) !! (s - 1))
   ++ map         F.D     ((allFs ps ns rs) !! (s - 1))
   ++ map         F.Neg   (filter noNeg ((noTrivAllFs ps ns rs) !! (s - 1)))
   ++ if s > 1
       then let half      = s `div` 2 ; remainder = s `mod` 2  in
            cartProdApp (F.:&:) ((noTrivAllFs ps ns rs) !! half)                    -- not
                                ((noTrivAllFs ps ns rs) !! (half + remainder))      -- exhaustive
       else []
  , s + 1)

cartProdApp :: (a -> b -> c) -> [a] -> [b] -> [c]
cartProdApp f as bs = concatMap (\a -> map (f a) bs) as

noNeg :: F.Formula n p r -> Bool
noNeg (F.Neg _) = False
noNeg _ = True

-- To avoid trivialities (top, bot) in conjunctions :

noTrivAllFs :: [PropSymbol] -> [NomSymbol] -> [RelSymbol] -> [[F.Formula NomSymbol PropSymbol RelSymbol]]
noTrivAllFs ps ns rs = (fsSizeOneNoTrivial ps ns rs):(tail $ allFs ps ns rs)

fsSizeOneNoTrivial :: [PropSymbol] -> [NomSymbol] -> [RelSymbol] -> [F.Formula NomSymbol PropSymbol RelSymbol]
fsSizeOneNoTrivial ps ns _ = map F.Prop ps ++ map F.Nom ns 



