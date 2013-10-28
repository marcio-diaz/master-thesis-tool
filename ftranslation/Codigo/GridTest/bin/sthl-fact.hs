#!/usr/bin/env runghc

-- or compile it with : ghc --make  -static -optl-static -optl-pthread sthl-fact

-- this program converts a hgen-generated hybrid formula in the simple format
-- to a description logic formula in the format used by FaCT++

module Main(main) where

import System(getArgs)
import System.IO
import Control.Applicative ( (<$>) )

import qualified Data.Set as Set
import HyLo.InputFile( parseOldFormat )
import HyLo.Formula ( Formula(..) )

import HyLo.Signature ( HasSignature(..), relSymbols, propSymbols, nomSymbols)
import HyLo.Signature.Simple ( NomSymbol, PropSymbol, RelSymbol )

main :: IO ()
main =
   do
        args <- getArgs
        inputFormula <- conjList <$> parseOldFormat <$> (readFile $ head args)
        let sig        = getSignature inputFormula
        let relations  = map show $ Set.toList $ relSymbols sig
        let predicates = map show $ Set.toList $ propSymbols sig
        let nominals   = map show $ Set.toList $ nomSymbols sig
        let (transF, needsTotalRel)  = tr inputFormula

        writeFile "TEST.tbox"
           $      defPrimConcepts predicates
              ++  defPrimRoles relations
              ++  "(defconcept Proof "
              ++  if not needsTotalRel
                    then transF ++ ")\n"
                    else    "(and (one-of NI) " ++ transF ++ "))\n"
                         ++ "(transitive TotalRel)\n (reflexive TotalRel)\n"
                         ++ totalRelRel relations ++ "\n"
                         ++ totalRelNom ("NI":nominals)   -- NI : initial nominal

type NeedsTotalRel = Bool

tr ::  Formula NomSymbol PropSymbol RelSymbol ->  (String,NeedsTotalRel)
tr Top            = ( "(or C (neg C))"                          , False    )
tr Bot            = ( "(and C (neg C))"                         , False    )
tr (Prop p)       = ( show p                                    , False    )
tr (Nom n)        = ( "(one-of " ++ show n ++ ")"               , False    )
tr (Neg f1)       = ( "(not " ++ t1 ++ ")"                      , b1       ) where (t1,b1) = tr f1
tr (f1 :|: f2)    = ( "(or " ++ t1 ++ " " ++ t2 ++ ")"          , b1 || b2 ) where (t1,b1) = tr f1 ; (t2,b2) = tr f2
tr (f1 :&: f2)    = ( "(and " ++ t1 ++ " " ++ t2 ++ ")"         , b1 || b2 ) where (t1,b1) = tr f1 ; (t2,b2) = tr f2
tr (f1 :-->: f2)  = ( "(or (not " ++ t1 ++ ") " ++ t2 ++ ")"    , b1 || b2 ) where (t1,b1) = tr f1 ; (t2,b2) = tr f2
tr (f1 :<-->: f2) = ( "(and (or (not " ++ t1 ++ ") " ++ t2 ++ ") "
                      ++ "(or (not " ++ t2 ++ ") " ++ t1 ++ "))", b1 || b2 ) where (t1,b1) = tr f1 ; (t2,b2) = tr f2
tr (Diam r f1)    = ( "(some " ++ show r ++ " " ++ t1 ++ ")"    , b1       ) where (t1,b1) = tr f1
tr (Box r f1)     = ( "(all " ++ show r ++ " " ++ t1 ++ ")"     , b1       ) where (t1,b1) = tr f1
tr (At n f1)      = ( "(some TotalRel (and " ++
                      "(one-of " ++ show n ++ ") " ++ t1 ++ "))", True     ) where (t1,_) = tr f1
tr (A f1)         = ( "(all TotalRel " ++ t1 ++ ")"             , True     ) where (t1,_) = tr f1
tr (E f1)         = ( "(some TotalRel " ++ t1 ++ ")"            , True     ) where (t1,_) = tr f1

totalRelRel :: [String] -> String
totalRelRel ss = let f s current = current ++ "(implies_r " ++ s ++ " TotalRel)\n" in
                 foldr f "(implies_r (inv TotalRel) TotalRel)\n" ss

totalRelNom :: [String] -> String
-- force all nominals to be linked to each others (including themselves, thus reflexivity is enforced)
totalRelNom [] = ""
totalRelNom (s1:ss) =
 go s1 (s1:ss) ++ totalRelNom ss
   where go s1 [] = ""
         go s1 (s2:ss) = s1 `linkTo` s2 ++ go s1 ss
                         where linkTo s1 s2
                                = (if s1 == s2 then "" else "(related " ++ s2 ++ " TotalRel " ++ s1 ++ ")\n")
                                                         ++ "(related " ++ s1 ++ " TotalRel " ++ s2 ++ ")\n"

defPrimConcepts :: [String] -> String
defPrimConcepts ss = let f s current = current ++ "(defprimconcept " ++ s ++ ")\n" in foldr f "" ss

defPrimRoles :: [String] -> String
defPrimRoles ss = let f s current = current ++ "(defprimrole " ++ s ++  ")\n" in foldr f "" ss

conjList :: [Formula NomSymbol PropSymbol RelSymbol] -> Formula NomSymbol PropSymbol RelSymbol
conjList = foldr1 (:&:)

