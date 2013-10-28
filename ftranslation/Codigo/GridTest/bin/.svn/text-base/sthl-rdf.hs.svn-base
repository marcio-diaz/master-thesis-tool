#!/usr/bin/env runghc

-- or compile it with : ghc --make sthl-rdf.hs

-- this program converts a hgen-generated hybrid formula to a
-- description logic formula in the OWL format used by Pellet

module Main(main) where


import System(getArgs)
import System.IO     
import Control.Applicative ( (<$>) )

import HyLo.InputFile( parse )
import HyLo.Formula ( Formula(..) )

import HyLo.Signature ( HasSignature(..), HasVariables(..) , relSymbols, propSymbols, nomSymbols)
import HyLo.Signature.Simple ( NomSymbol, PropSymbol, RelSymbol, StateVar )

main :: IO ()
main = 
   do
        args <- getArgs
        putStr (show args)
        inputFormula <- conjList <$> parse <$> (readFile $ head args)
        let sig        = getSignature inputFormula
        let relations  = map show $ Set.toList $ relSymbols sig
        let predicates = map show $ Set.toList $ propSymbols sig
        let nominals   = map show $ Set.toList $ nomSymbols sig
        let (transF, needsTotalRel)  = tr inputFormula
          
        let header = "<!DOCTYPE rdf:RDF [\n <!ENTITY xsd 'http://www.w3.org/2001/XMLSchema#'> \n <!ENTITY owl 'http://www.w3.org/2002/07/owl#'> \n <!ENTITY ns 'http://www.translation/proof'>]> \n <rdf:RDF xmlns:rdfs='http://www.w3.org/2000/01/rdf-schema#'\n xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#' \n xmlns:owl='http://www.w3.org/2002/07/owl#'\n  xmlns:owl11='http://www.w3.org/2006/12/owl11#' \n xmlns:ns='&ns;#' \n 	xml:base='&ns;'> \n <owl:Ontology rdf:about=''/>\n"

 
        writeFile "proof.owl" (concat [(header ++ "<owl:Class rdf:ID='proof'>\n" ++ "<owl:equivalentClass>\n" {-++ "<owl:Class>\n"-}), if elem "rdf:about='#TotalRel'/>" (words(concat (map translation inputFormula)))) then (concat ["<owl:Class>" ++ "\n" ++ "<owl:intersectionOf rdf:parseType='Collection'>" ++ "\n", "<owl:Class>" ++ "\n" ++ "<owl:oneOf rdf:parseType='Collection'>" ++ "\n" ++ "<owl:Thing rdf:about='#NI'/>" ++ "</owl:oneOf>" ++ "\n" ++ "</owl:Class>" ++ "\n", concat(map translation (concatF inputFormula)),"</owl:intersectionOf>" ++ "\n" ++ "</owl:Class>" ++ "\n",({-"</owl:Class>\n" ++-} "</owl:equivalentClass>" ++ "\n" ++ "</owl:Class>" ++ "\n"), (concat["<owl11:ReflexiveProperty rdf:ID='TotalRel'>\n <rdf:type rdf:resource='&owl;SymmetricProperty'/>\n <rdf:type rdf:resource='&owl;TransitiveProperty'/>\n","</owl11:ReflexiveProperty>\n" {-transitive and symmetric-}
        {-In the previous version, the reflexive property was obtained by saying that the total rel. is transitive, symmetric, and that its cardinality es grater or igual thatn 1, but now, in owl 1.1 it there is a speciall instruction for saying that-},"\n",(concat (map totalRelRel (map eliminaRep (map getRelations (concatF inputFormula))))),"\n",(concat (map totalRelNom (map eliminaRep (map getNominals (concatF inputFormula)))))]),"\n"]) else (concat[(concat (map translation (concatF inputFormula))),"\n",({-"</owl:Class>\n" ++-} "</owl:equivalentClass>" ++ "\n" ++ "</owl:Class>" ++ "\n")]), (concat (map defPrimConcept (map eliminaRep (map getPredicates (concatF inputFormula))))),
	     (concat (map defPrimRole (map eliminaRep (map getRelations (concatF inputFormula))))),
	     (concat (map defNominals (map eliminaRep (map getNominals (concatF inputFormula))))),
	     "</rdf:RDF>"])

        hClose stdout


translation ::  Formula NomSymbol PropSymbol RelSymbol StateVar ->  String
translation Top      = "<owl:Thing/> \n"
translation Bot      = "<owl:Nothing/> \n"
translation (Prop p) = "<owl:Class rdf:about='#" ++  show p ++ "'/>\n"
translation (Nom n)  = "<owl:Class>\n<owl:oneOf rdf:parseType='Collection'>\n<owl:Thing rdf:about='#"
                       ++ show n ++ "'/></owl:oneOf>\n</owl:Class>\n"
translation (Neg f1) = "<owl:Class>\n<owl:complementOf>\n" ++ translation f1
                       ++ "</owl:complementOf>\n</owl:Class>\n"  
translation (f1 :|: f2) = concat ["<owl:Class>" ++ "\n" ++ "<owl:unionOf rdf:parseType='Collection'>"++"\n",
				  translation f1, "\n", translation f2,
                                  "</owl:unionOf>" ++ "\n" ++ "</owl:Class>" ++ "\n"]  
translation (f1 :&: f2) = concat ["<owl:Class>" ++ "\n" ++ "<owl:intersectionOf rdf:parseType='Collection'>" ++ "\n", 	
			  translation f1, "\n", translation f2,
			  "</owl:intersectionOf>" ++ "\n" ++ "</owl:Class>" ++ "\n"]  
translation (f1 :-->: f2) = concat ["<owl:Class>" ++ "\n" ++ "<owl:unionOf rdf:parseType='Collection'>",
	    "<owl:Class>" ++ "\n" ++ "<owl:complementOf>" ++ "\n", translation f1, "</owl:complementOf>" ++ "\n" ++ "</owl:Class>" ++ "\n", 
            translation f2,"</owl:unionOf>" ++ "\n" ++ "</owl:Class>" ++ "\n"]
translation (f1 :<-->: f2) = concat ["<owl:Class>" ++ "\n" ++ "<owl:intersectionOf rdf:parseType='Collection'>" ++ "\n" ++ "<owl:Class>" ++ "\n" ++ "<owl:unionOf rdf:parseType='Collection'>" ++ "<owl:Class>" ++ "\n" ++ "<owl:complementOf>" ++ "\n", translation f1, "</owl:complementOf>" ++ "\n" ++ "</owl:Class>" ++ "\n", translation f2, "</owl:unionOf>" ++ "\n" ++ "</owl:Class>"++ "\n" ++ "<owl:Class>" ++ "\n" ++ "<owl:unionOf rdf:parseType='Collection'>" ++ "<owl:Class>" ++ "\n" ++ "<owl:complementOf>" ++ "\n", translation f2, "</owl:complementOf>" ++ "\n" ++ "</owl:Class>" ++ "\n", translation f1,"</owl:unionOf>" ++ "\n" ++ "</owl:Class>" ++ "\n"++"</owl:intersectionOf>" ++ "\n" ++ "</owl:Class>" ++ "\n"]
translation (Diam r (f1)) = concat ["<owl:Restriction>" ++ "\n" ++ "<owl:onProperty rdf:resource='#" ++ show r ++ "'/>" ++ "<owl:someValuesFrom>" ++ "\n" , translation f1, "</owl:someValuesFrom>" ++ "\n" ++ "</owl:Restriction>" ++ "\n"]
translation (Box r (f1)) = concat ["<owl:Restriction>" ++ "\n" ++ "<owl:onProperty rdf:resource='#" ++ show r ++ "'/>" ++ "<owl:allValuesFrom>" ++ "\n" , translation f1, "</owl:allValuesFrom>" ++ "\n" ++ "</owl:Restriction>" ++ "\n"]

translation (At n (f1)) = concat [{-"<owl:Class> \n <owl:intersectionOf rdf:parseType='Collection'>\n" ++ "<owl:Restriction>\n" ++ "<owl:minCardinality rdf:datatype='&xsd;integer'>1</owl:minCardinality>\n" ++  "<owl:onProperty> \n <owl:ObjectProperty rdf:about='#TotalRel'/> \n </owl:onProperty> \n" ++ "</owl:Restriction> \n" ++  -}"<owl:Restriction>\n" ++ "<owl:onProperty> \n <owl:ObjectProperty rdf:about='#TotalRel'/> \n </owl:onProperty> \n" ++ "<owl:someValuesFrom>" ++ "\n" ++ "<owl:Class>" ++ "\n" ++ "<owl:intersectionOf rdf:parseType='Collection'>" ++ "\n" ++ "<owl:Class>" ++ "\n" ++ "<owl:oneOf rdf:parseType='Collection'>" ++ "\n" ++ "<owl:Thing rdf:about='#" ++ show n ++ "'/>" ++ "</owl:oneOf>" ++ "\n" ++ "</owl:Class>" ++ "\n", translation f1, "</owl:intersectionOf>" ++ "\n" ++ "</owl:Class>" ++  "\n" ++ "</owl:someValuesFrom>" ++ "\n" ++ "</owl:Restriction> \n" {-++"</owl:intersectionOf>\n</owl:Class>\n"-} ]
--["(some TotalRel (and (one-of ", show n,") ", translation f1  , "))"]
translation (A (f1)) = concat [{-"<owl:Class> \n <owl:intersectionOf rdf:parseType='Collection'>\n" ++ "<owl:Restriction>\n" ++ "<owl:minCardinality rdf:datatype='&xsd;integer'>1</owl:minCardinality>\n" ++  "<owl:onProperty> \n <owl:ObjectProperty rdf:about='#TotalRel'/> \n </owl:onProperty> \n" ++ "</owl:Restriction> \n" ++ -}"<owl:Restriction>" ++ "\n" ++ "<owl:onProperty> \n <owl:ObjectProperty rdf:about='#TotalRel'/> \n </owl:onProperty> \n" ++ "<owl:allValuesFrom>" ++ "\n" , translation f1, "</owl:allValuesFrom>" ++ "\n" ++ "</owl:Restriction>\n" {-++"</owl:intersectionOf>\n</owl:Class>\n"-}]
--["(all TotalRel ", translation f1, ")"]
translation (E (f1)) = concat [ {-"<owl:Class> \n <owl:intersectionOf rdf:parseType='Collection'>\n" ++ "<owl:Restriction>\n" ++ "<owl:minCardinality rdf:datatype='&xsd;integer'>1</owl:minCardinality>\n" ++  "<owl:onProperty> \n <owl:ObjectProperty rdf:about='#TotalRel'/> \n </owl:onProperty> \n" ++ "</owl:Restriction> \n" ++ -}"<owl:Restriction>" ++ "\n" ++ "<owl:onProperty> \n <owl:ObjectProperty rdf:about='#TotalRel'/> \n </owl:onProperty> \n" ++ "<owl:someValuesFrom>" ++ "\n" , translation f1, "</owl:someValuesFrom>" ++ "\n" ++ "</owl:Restriction>\n" {-++"</owl:intersectionOf>\n</owl:Class>\n"-}]
--["(some TotalRel ", translation f1, ")"]

getRelations :: Formula NomSymbol PropSymbol RelSymbol StateVar ->  [String]
getRelations f = map show $ Set.toList $ relSymbols $ getSignature f

getNominals :: Formula NomSymbol PropSymbol RelSymbol StateVar ->  [String]
getNominals f = map show $ Set.toList $ nomSymbols $ getSignature f

getPredicates :: Formula NomSymbol PropSymbol RelSymbol StateVar ->  [String]
getPredicates f = map show $ Set.toList $ propSymbols $ getSignature f

totalRelRel :: [String] -> String
totalRelRel (x:xs) = concat["<owl:ObjectProperty rdf:ID='",x,"'>", "\n", "<rdfs:subPropertyOf rdf:resource='#TotalRel'/>", "\n", "</owl:ObjectProperty>", "\n", totalRelRel xs]
--["(implies_r ",  x," TotalRel)\n", totalRelRel xs]
totalRelRel [] = ""
--"<owl:ObjectProperty rdf:ID='invTR'>\n<owl:inverseOf rdf:resource='#TotalRel'/>\n</owl:ObjectProperty>\n" ++ --"<owl:ObjectProperty rdf:ID='invTR'>\n<rdfs:subPropertyOf rdf:resource='#TotalRel'/>\n</owl:ObjectProperty>\n"
--"(implies_r (inv TotalRel) TotalRel)\n"


totalRelNom :: [String] -> String
totalRelNom (x:xs) = concat["<owl:Thing rdf:about='#", x, "'>\n <ns:TotalRel rdf:resource='#", x,"'/>\n </owl:Thing>\n",
			    totalRelNom2 x xs, totalRelNom xs]

--["(related ",  x , " TotalRel ",  x,  " ) " , "\n" ,totalRelNom2 x xs, totalRelNom xs]
totalRelNom [] = "<owl:Thing rdf:about='#NI'>\n <ns:TotalRel rdf:resource='#NI'/>\n </owl:Thing>\n"
--"(related NI TotalRel NI) \n"


totalRelNom2 :: String -> [String] -> String
totalRelNom2 a (x:xs) = concat["<owl:Thing rdf:about='#", a, "'>\n <ns:TotalRel rdf:resource='#", x,"'/>\n </owl:Thing>\n <owl:Thing rdf:about='#", x, "'>\n <ns:TotalRel rdf:resource='#", a,"'/>\n </owl:Thing>\n", totalRelNom2 a xs]

--["(related ",  a , " TotalRel ",  x, " ) ", "\n" , "(related ",  x , " TotalRel ",  a, " ) ",  "\n" , totalRelNom2 a xs]
totalRelNom2 a [] = concat ["<owl:Thing rdf:about='#NI'>\n <ns:TotalRel rdf:resource='#", a,"'/>\n </owl:Thing>\n <owl:Thing rdf:about='#", a, "'>\n <ns:TotalRel rdf:resource='#NI'/>\n </owl:Thing>\n"]
--["(related NI TotalRel ",  a, " ) ", "\n" , "(related ",  a , " TotalRel NI) ",  "\n" ]

eliminaRep :: (Ord t) => [t] -> [t]

eliminaRep [] = []
eliminaRep [a]       = [a]
eliminaRep (a:b:x)
       | elem a (b:x)      = eliminaRep (b:x)
       | otherwise   = a : eliminaRep (b:x)

defPrimConcept :: [String] -> String
defPrimConcept (x:xs) = concat["<owl:Class rdf:ID='" ++ x ++ "'/>" ++ "\n", defPrimConcept xs]
--["(defprimconcept ",  x,")\n", defPrimConcept xs]
defPrimConcept [] = ""

defPrimRole :: [String] -> String
defPrimRole (x:xs) = concat["<owl:ObjectProperty rdf:ID='" ++ x ++ "'/>" ++ "\n", defPrimRole xs]
--["(defprimrole ",  x,")\n", defPrimRole xs]
defPrimRole [] = ""

defNominals :: [String] -> String
defNominals (x:xs) = concat["<owl:Thing rdf:about='#" ++ x ++ "'>" ++ "\n" ++ "<rdf:type rdf:resource='http://www.w3.org/2002/07/owl#Thing'/>" ++ "\n" ++ "</owl:Thing>" ++ "\n", defNominals xs]
--["(defprimrole ",  x,")\n", defPrimRole xs]
defNominals [] = ""

concatF :: [Formula NomSymbol PropSymbol RelSymbol StateVar] -> [Formula NomSymbol PropSymbol RelSymbol StateVar]
concatF (f1:[]) = [f1]
concatF (f1:f2) = [f1 :&: (head (concatF f2))]
--concatF [] = []

conjList :: [Formula NomSymbol PropSymbol RelSymbol StateVar] -> Formula NomSymbol PropSymbol RelSymbol StateVar
conjList = foldr1 (:&:)
