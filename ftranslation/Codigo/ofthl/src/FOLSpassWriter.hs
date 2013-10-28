module FOLSpassWriter

where

import Data.List(nub, intersperse)

import OFTranslation

data SpassProblemDescr = Problem{problemId   :: String,
                                 name        :: String,
                                 author      :: String,
                                 logic       :: Maybe String,
                                 status      :: FormulaStatus,
                                 description :: String}

data FormulaStatus      = Sat | Unsat | Unknown

instance Show FormulaStatus where
  show Sat     = "satisfiable"
  show Unsat   = "unsatisfiable"
  show Unknown = "unknown"

spassFormat :: SpassProblemDescr -> [FOL] -> String
spassFormat descr fs = concat ["begin_problem(", (problemId descr), ").\n\n",
                                (header descr), "\n\n",
                                (signature fs), "\n\n",
                                (formulas fs), "\n\n",
                               "end_problem."]

header :: SpassProblemDescr -> String
header d = concat [" list_of_descriptions.\n",
                    "  name({*", name d, "*}).\n",
                    "  author({*", author d, "*}).\n",
                    maybe "" (\l -> concat ["  logic({*", l,  "*}).\n"]) (logic d),
                    "  status(", show . status $ d, ").\n",
                    "  description({*", description d, "*}).\n",
                    " end_of_list."]

signature :: [FOL] -> String
signature fs = concat [" list_of_symbols.\n",
                       if null funcSigs then "functions[(X1, 0), (R1, 2)].\n" else ("  functions[(X1, 0), (R1, 2),"  ++ (showSs funcSigs) ++ "].\n"),
--                       if null funcSigs then "functions[(a, 0)].\n" else ("  functions[(a, 0)," ++ (showSs funcSigs) ++ "].\n"),
--                        if null funcSigs then "" else ("  functions[ " ++ (showSs funcSigs) ++ "].\n"),
			if null predSigs then  "predicates[(P1,1),(P2,1),(P3,1)].\n" else ("  predicates[" ++ "(P1,1),(P2,1),(P3,1)," ++ (showSs predSigs) ++ "].\n"),
--                        if null predSigs then  "" else ("  predicates[" ++ (showSs predSigs) ++ "].\n"),
                        " end_of_list."]
    where showS (a,b) = concat ["(", a, ", ", show b, ")"]
          showSs      = concat . intersperse ", " . map showS
          funcSigs    = (funcSig fs) ++ (nominalSig nom fs)
          predSigs    = (p1sSig fs) ++ (r2sSig fs)
          
nominalSig :: (NomId -> String) -> [FOL] -> [(String, Int)]
nominalSig f = map (flip (,) 0 . f) . nub . concatMap nomsOccurringFOL

p1sSig :: [FOL] -> [(String, Int)]
p1sSig = pXsSig p1sOccurringFOL 1

r2sSig :: [FOL] -> [(String, Int)]
r2sSig = rXsSig r2sOccurringFOL 2

pXsSig :: (FOL -> [PropId]) -> Int -> [FOL] -> [(String, Int)]
pXsSig f arity = map (flip (,) arity . relP) . nub . concatMap f

rXsSig :: (FOL -> [RelId]) -> Int -> [FOL] -> [(String, Int)]
rXsSig f arity = map (flip (,) arity . relR) . nub . concatMap f


funcSig :: [FOL] -> [(String, Int)]
funcSig = map (flip (,) 2) . nub . concatMap funcOccurring


formulas :: [FOL] -> String
formulas fs = concat [" list_of_formulae(axioms).\n",

--formula(forall([X], and(not(Pde1(X)), exists([Z], equal(R1(Z,X), X))))).\n",

--formula(forall([X], implies(P1(X), exists([Y], and(not(Pde1(X)),and(exists([Z],equal(R1(Z,X),Y)),P1(Y))))))).\nformula(forall([X], implies(P2(X), exists([Y], and(not(Pde1(X)),and(exists([Z],equal(R1(Z,X),Y)),P2(Y))))))).\nformula(forall([X], implies(P3(X), exists([Y], and(not(Pde1(X)),and(exists([Z],equal(R1(Z,X),Y)),P3(Y))))))).\n",


--formula(forall([X], implies(not(Pde1(X)), forall([Y], forall([W], exists([Z], equal(R1(W, R1(Y, X)), R1(Z, X)))))))).\n",


                       concatMap (\f -> concat ["  formula(", formula f, ").\n"]) fs,
                       " end_of_list."]

formula :: FOL -> String
formula (l :== r)     = concat ["equal(", term l,  ", ", term r,  ")"]
formula (P i x)     = concat [relP i, "(", term x, ")"]
formula (R i x y)   = concat [relR i, "(", term x, ", ", term y, ")"]
formula (Not f)      = concat ["not(", formula f, ")"]
formula (l :&& r)     = concat ["and(", formula l, ", ", formula r, ")"]
formula (l :|| r)     = concat ["or(", formula l, ", ", formula r, ")"]
formula (l :--> r)    = concat ["implies(", formula l, ", ", formula r, ")"]
formula (l :<--> r)   = concat ["equiv(", formula l, ", ", formula r, ")"]
formula (E v f)    = concat ["exists([", printVar v, "], ", formula f, ")"]
formula (A v f)    = concat ["forall([", printVar v, "], ", formula f, ")"]

funcF, constC :: String
funcF = "f"
constC = "c"



funcOccurring :: FOL -> [String]
funcOccurring (t1 :== t2) = (funcInTerm t1) ++ (funcInTerm t2)
funcOccurring (P _ t) = funcInTerm t
funcOccurring (R _ t1 t2) = (funcInTerm t1) ++ (funcInTerm t2)
funcOccurring (Not f) = funcOccurring f
funcOccurring (f1 :&& f2) = (funcOccurring f1) ++ (funcOccurring f2)
funcOccurring (f1 :|| f2) = (funcOccurring f1) ++ (funcOccurring f2)
funcOccurring (f1 :--> f2) = (funcOccurring f1) ++ (funcOccurring f2)
funcOccurring (f1 :<--> f2) = (funcOccurring f1) ++ (funcOccurring f2)
funcOccurring (E _ f) = (funcOccurring f)
funcOccurring (A _ f) = (funcOccurring f)

funcInTerm :: Term -> [String]
funcInTerm (V _) = []
funcInTerm (N _) = []
funcInTerm (F r i t) = [(relR r)] ++ (funcInTerm t)


term :: Term -> String
term (V i) = printVar i
term (N n)     = nom n
term (F r i t) = (relR r) ++ "(" ++ ("X"++(show i)) ++ ", " ++ term t ++ ")"
term  unknown  = error $ "Unexpected term: " ++ (show unknown)

nom :: NomId -> String
nom  = ("N" ++)

relR :: RelId -> String
relR = ('R':)

relP :: PropId -> String
relP = ('P':)
