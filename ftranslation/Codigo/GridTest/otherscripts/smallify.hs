#! /usr/bin/env runghc

-- ==========================================================================
--
--       Smallify
--
--       given a formula and two inconsistent provers result,
--       (or one prover and an expected result)
--       find a smaller formula maintaining the inconsistency
--
--       the input formula has to be in te "old format"
--       it does it quite stupidly by:
--         deleting lines
--         replacing nominals, propositional symbols, relations by others
--           (when there are several)
--         replacing nominals, props by true or false
--
--       examples of use :
--
--       ./smallify.hs file.frm "./htab" "unsatisfiable" "satisfiable" "./hylores" "unsatisfiable" "satisfiable" 
--       ./smallify.hs file.frm "./htab" "unsatisfiable" "satisfiable"  SAT "" ""
--       ./smallify.hs file.frm UNSAT "" "" "./htab" "unsatisfiable "satisfiable"
--
--       the reduced formula will be outputed in file.frm.reduced
--
-- ==========================================================================


import qualified HyLo.InputFile as InputFile
import qualified HyLo.Formula as F
import HyLo.Signature ( HasSignature(..), propSymbols, nomSymbols, relSymbols )
import HyLo.Signature.Simple ( NomSymbol, PropSymbol, RelSymbol )

import qualified Data.Set as Set
import Data.List ( isPrefixOf, isInfixOf, minimumBy )

import System.Process ( readProcessWithExitCode )
import System.Environment ( getArgs, getProgName )
import System.Directory ( doesFileExist , getPermissions, Permissions (..) )

import Control.Applicative ( (<$>) )
import Control.Monad ( when, filterM )

import Debug.Trace

data Comparator = Prover FilePath String String | SAT | UNSAT  deriving (Eq,Show)

main :: IO ()
main =
 do args <- getArgs
    if length args /= 7
     then showHelp
     else do let [file,cmd1,unsat1,sat1,cmd2,unsat2,sat2] = args
             cmp1 <- readComparator cmd1 unsat1 sat1
             cmp2 <- readComparator cmd2 unsat2 sat2
             case (cmp1,cmp2) of {(Just c1, Just c2) -> start file c1 c2 ; _ -> showHelp}

readComparator :: String -> String -> String -> IO (Maybe Comparator)
readComparator "SAT"   _ _ = return $ Just SAT
readComparator "UNSAT" _ _ = return $ Just UNSAT
readComparator cmd unsat sat
 = do return $ if any null [cmd,unsat,sat]
                 then Nothing
                 else Just $ Prover cmd unsat sat

-- start : lit la formule
-- loop: pour chaque règle, liste les bonnes applications possibles* à la formula, concatène tout
--       prend la tête de la liste, recommence dessus (ie : profondeur d'abord) 
--        "bonnes application possible" = applications possibles qui sortent un contre-exemple
-- pour l'instant on ne cherche pas à parcourir tout l'espace de recherche

start :: FilePath -> Comparator -> Comparator -> IO ()
start file cmp1 cmp2 =
 do content <- readFile file
    let formula = foldr1 (\f1 f2 -> f1 F.:&: f2) $ InputFile.parseOldFormat content
    resultFormulas <- loop formula cmp1 cmp2
    let resultFormula = smallest resultFormulas
    writeFormulaInFile resultFormula (file ++ ".reduced")
    putStrLn "Done"

smallest :: Show a => [a] -> a
smallest as = snd $ minimumBy compareLengths lenShows
 where lenShows = map (\a -> (length $ show a,a)) as
       compareLengths (l1,_) (l2,_) =  compare l1 l2


loop :: HyLoFormula -> Comparator -> Comparator -> IO [HyLoFormula]
loop formula cmp1 cmp2 =
 do possibilities <- applicableRules formula cmp1 cmp2
    case possibilities of
     []     -> return [formula]
     (hd:_) -> do loopOut <- loop hd cmp1 cmp2
                  concat <$> mapM (\f -> loop f cmp1 cmp2) loopOut

data Rule = NNFize | ReplaceProp | ReplaceNom | ReplaceRel | RemoveConjunct
allRules = [NNFize, ReplaceProp, ReplaceNom, ReplaceRel, RemoveConjunct]

applicableRules :: HyLoFormula -> Comparator -> Comparator -> IO [HyLoFormula]
applicableRules formula cmp1 cmp2 = filterM (isInconsistent cmp1 cmp2) $ concatMap (applicableRule formula) allRules

isInconsistent :: Comparator -> Comparator -> HyLoFormula -> IO Bool
isInconsistent cmp1 cmp2 f
  = do writeFormulaInFile f "tmp"
       res1 <- execute cmp1
       res2 <- execute cmp2
       return $ res1 /= res2 

data Result = R_SAT | R_UNSAT deriving (Eq)

execute :: Comparator -> IO Result 
execute SAT   = return R_SAT
execute UNSAT = return R_UNSAT
execute (Prover prover unsat sat) =
 do outStr <- runProver prover
    if unsat `isInfixOf` outStr then return R_UNSAT
     else if sat `isInfixOf` outStr then return R_SAT
      else error "error executing prover"

applicableRule :: HyLoFormula -> Rule -> [HyLoFormula]
applicableRule f NNFize = filter (/= f) [F.nnf f]

applicableRule f ReplaceProp =
 case Set.toList $ propSymbols $ getSignature f of
    props@(_:_:_) -> map (\c -> replaceProp c f) $ allCouples props
    _             -> []

applicableRule f ReplaceNom =
 case Set.toList $ nomSymbols $ getSignature f of
    noms@(_:_:_) -> map (\c -> replaceNom c f) $ allCouples noms
    _            -> []

applicableRule f ReplaceRel =
 case Set.toList $ relSymbols $ getSignature f of
    rels@(_:_:_) -> map (\c -> replaceRel c f) $ allCouples rels
    _            -> []

applicableRule f RemoveConjunct =
 case f of
  f1 F.:&: f2 -> (applicableRule f2 RemoveConjunct) ++ (applicableRule f1 RemoveConjunct) ++ [f2,f1]
  _           -> []

replaceProp :: (PropSymbol,PropSymbol) -> HyLoFormula -> HyLoFormula
replaceProp (p1,p2) (F.Prop p) = if p1 == p then F.Prop p2 else F.Prop p
replaceProp (p1,p2) f = F.composeMap id (replaceProp (p1,p2)) f

replaceNom :: (NomSymbol,NomSymbol) -> HyLoFormula -> HyLoFormula
replaceNom (n1,n2) (F.Nom n) = if n1 == n then F.Nom n2 else F.Nom n
replaceNom (n1,n2) f = F.composeMap id (replaceNom (n1,n2)) f

replaceRel :: (RelSymbol,RelSymbol) -> HyLoFormula -> HyLoFormula
replaceRel (r1,r2) (F.Diam r f) = if r1 == r then F.Diam r2 f else F.Diam r $ replaceRel (r1,r2) f
replaceRel (r1,r2) f = F.composeMap id (replaceRel (r1,r2)) f


showHelp :: IO ()
showHelp =
 do name <- getProgName
    putStrLn $ unlines
     ["Usage : " ++ name ++ " formula.frm \"prover1\" \"unsat_string\" \"sat_string\" \"prover2\" \"unsat_string\" \"sat_string\""]

runProver :: String -> IO String
runProver cmd_ = do (_,stdout,_) <- readProcessWithExitCode cmd params ""
                    return stdout
 where (cmd:params) = words cmd_

allCouples :: [a] -> [(a,a)]
allCouples (x:xs) = allCouples xs ++ concatMap (\y -> [(y,x),(x,y)]) xs
allCouples _ = []

-- ==========================================================================

type HyLoFormula = F.Formula NomSymbol PropSymbol RelSymbol


writeFormulaInFile :: (Show n , Show p, Show r) => F.Formula n p r -> String -> IO ()
writeFormulaInFile f file =
  writeFile file $ unlines ["begin\n", show f, "\nend"]


-- substring replacement
replace::(Eq a) => [a] -> [a] -> [a] -> [a]
replace _  _ [] = []
replace [] _ _  = []
replace oldSub newSub list@(hd:tl)
 = if isPrefixOf oldSub list
    then newSub ++ replace oldSub newSub (drop (length oldSub) list)
    else hd : (replace oldSub newSub tl)

