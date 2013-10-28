module FOLSpassWriter2

where

import Data.List(nub, intersperse)
import Data.Set(fromList, toList)
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

spassFormat :: SpassProblemDescr -> [SFOL] -> String
spassFormat descr fs = concat ["begin_problem(", (problemId descr), ").\n\n",
                                (header descr), "\n\n",
                                (signature fs), "\n\n",
                                "list_of_declarations.\n",
                                "end_of_list.\n\n",
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

signature :: [SFOL] -> String
signature fs = concat [" list_of_symbols.\n",
                       if null funcSigs then "  functions[(b, 0), (X1, 0)]." else ("  functions[(b, 0), (X1, 0)," ++ (indexVars fs) ++ (showSs funcSigs) ++"].\n"),
--                       if null funcSigs then "  functions[(a, 0), (b, 0)]." else ("  functions[(a, 0), (b, 0), " ++ (showSs funcSigs) ++ "].\n"),
--                       if null funcSigs then "  functions[(X1, 0), (b, 0)]." else ("  functions[(X1, 0), (b, 0), " ++ (showSs funcSigs) ++ "].\n"),
                        if null predSigs then  "" else ("  predicates[" ++ (showSs predSigs) ++ "].\n"),
--                        if null sortSigs then  "" else ("  sorts[" ++ (sortSigs) ++ "].\n"),
                        "   sorts[omega, iota].\n",
                        " end_of_list."]
    where showS (a,b) = concat ["(", a, ", ", show b, ")"]
          showSs      = concat . intersperse ", " . map showS
          funcSigs    = (funcSig fs) ++ (nominalSig nom fs)
          predSigs    = (p1sSig fs) ++ (r2sSig fs)
          sortSigs    = (concat  $ intersperse ", "  (sortSig fs)) 
          
nominalSig :: (NomId -> String) -> [SFOL] -> [(String, Int)]
nominalSig f = map (flip (,) 0 . f) . nub . concatMap nomsOccurring

p1sSig :: [SFOL] -> [(String, Int)]
p1sSig = pXsSig p1sOccurring 1

r2sSig :: [SFOL] -> [(String, Int)]
r2sSig = rXsSig r2sOccurring 2

pXsSig :: (SFOL -> [PropId]) -> Int -> [SFOL] -> [(String, Int)]
pXsSig f arity = map (flip (,) arity . relP) . nub . concatMap f

rXsSig :: (SFOL -> [RelId]) -> Int -> [SFOL] -> [(String, Int)]
rXsSig f arity = map (flip (,) arity . relR) . nub . concatMap f


sortSig :: [SFOL] -> [String]
sortSig = map ssort . nub . concatMap sortOccurring

funcSig :: [SFOL] -> [(String, Int)]
funcSig = map (flip (,) 2) . nub . concatMap funcOccurring


formulas :: [SFOL] -> String
formulas fs = concat [" list_of_formulae(conjectures).\n",
                       concatMap (\f -> concat ["  formula(not(", formula f, ")).\n"]) fs,
                       " end_of_list."]

formula :: SFOL -> String
formula (l := r)     = concat ["equal(", term l,  ", ", term r,  ")"]
formula (P1 i x)     = concat [relP i, "(", term x, ")"]
formula (R2 i x y)   = concat [relR i, "(", term x, ", ", term y, ")"]
formula (Neg f)      = concat ["not(", formula f, ")"]
formula (l :& r)     = concat ["and(", formula l, ", ", formula r, ")"]
formula (l :| r)     = concat ["or(", formula l, ", ", formula r, ")"]
formula (l :-> r)    = concat ["implies(", formula l, ", ", formula r, ")"]
formula (l :<-> r)   = concat ["equiv(", formula l, ", ", formula r, ")"]
formula (Exi v s f)    = if (s == Index) then formula f
                         else concat ["exists([", printVar v, "], ", formula f, ")"]
formula (All v s f)    = concat ["forall([", printVar v, "], ", formula f, ")"]

getVars :: SFOL -> [Var]
getVars (Neg f)      = getVars f
getVars (l :& r)     = (getVars l) ++ (getVars r)
getVars (l :| r)     = (getVars l) ++ (getVars r)
getVars (l :-> r)    = (getVars l) ++ (getVars r)
getVars (l :<-> r)   = (getVars l) ++ (getVars r)
getVars (Exi v s f)  = if (s == Index) then v:(getVars f)
                       else (getVars f)
getVars (All v s f)  = getVars f
getVars _            = []

indexIotaVars :: [SFOL] -> String
indexIotaVars fs = concatMap (\v -> printIotaVar v) 
               (toList (fromList (allIotaVars fs)))

indexVars :: [SFOL] -> String
indexVars fs = concatMap (\v -> concat ["(",printVar v, ",0),"])
               (toList (fromList (allIotaVars fs)))


printIotaVar :: Var -> String
printIotaVar v = "iota(" ++ printVar v ++ ").\n"

allIotaVars :: [SFOL] -> [Var]
allIotaVars [] = []
allIotaVars (f:fs) = (getVars f) ++ (allIotaVars fs)

funcF, constC :: String
funcF = "f"
constC = "c"


sortOccurring :: SFOL -> [Sort]
sortOccurring (_ := _) = []
sortOccurring (P1 _ _) = []
sortOccurring (R2 _ _ _) = []
sortOccurring (Neg f) = sortOccurring f
sortOccurring (f1 :& f2) = (sortOccurring f1) ++ (sortOccurring f2)
sortOccurring (f1 :| f2) = (sortOccurring f1) ++ (sortOccurring f2)
sortOccurring (f1 :-> f2) = (sortOccurring f1) ++ (sortOccurring f2)
sortOccurring (f1 :<-> f2) = (sortOccurring f1) ++ (sortOccurring f2)
sortOccurring (Exi _ s f) = [s] ++ (sortOccurring f)
sortOccurring (All _ s f) = [s] ++ (sortOccurring f)


funcOccurring :: SFOL -> [String]
funcOccurring (t1 := t2) = (funcInTerm t1) ++ (funcInTerm t2)
funcOccurring (P1 _ t) = funcInTerm t
funcOccurring (R2 _ t1 t2) = (funcInTerm t1) ++ (funcInTerm t2)
funcOccurring (Neg f) = funcOccurring f
funcOccurring (f1 :& f2) = (funcOccurring f1) ++ (funcOccurring f2)
funcOccurring (f1 :| f2) = (funcOccurring f1) ++ (funcOccurring f2)
funcOccurring (f1 :-> f2) = (funcOccurring f1) ++ (funcOccurring f2)
funcOccurring (f1 :<-> f2) = (funcOccurring f1) ++ (funcOccurring f2)
funcOccurring (Exi _ _ f) = (funcOccurring f)
funcOccurring (All _ _ f) = (funcOccurring f)

funcInTerm :: Term -> [String]
funcInTerm (V _) = []
funcInTerm (N _) = []
funcInTerm (F r i t) = [(relR r)] ++ (funcInTerm t)

ssort :: Sort -> SortId
ssort World = "omega"
ssort Index = "iota"


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
