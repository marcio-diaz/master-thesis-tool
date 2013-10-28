module ModalSpassWriter

where

import Data.List(nub, intersperse)

import OFTranslation
import Prepo.HyLoAST
import Prepo.AST

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

spassFormat :: SpassProblemDescr -> [HyLoAST] -> String
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

signature :: [HyLoAST] -> String
signature fs = concat [" list_of_symbols.\n",
                        if null predSigs then  "" else ("predicates[" ++ (showSs predSigs) ++ "].\n"),
                        " end_of_list."]
    where showS (a,b) = concat ["(", a, ", ", show b, ")"]
          showSs      = concat . intersperse ", " . map showS
          predSigs    = (p1sSig fs) ++ (r2sSig fs)


p1sModOcurring :: HyLoAST -> [PropId]
p1sModOcurring (LV NodeProp p) = [p]
p1sModOcurring (N1 NodeNeg f) = p1sModOcurring f
p1sModOcurring (N2 NodeDia r f) = p1sModOcurring f
p1sModOcurring (N2 NodeBox r f) = p1sModOcurring f
p1sModOcurring (N2 NodeAnd l r) = (p1sModOcurring l) ++ (p1sModOcurring r)
p1sModOcurring (N2 NodeOr l r) = (p1sModOcurring l) ++ (p1sModOcurring r)
p1sModOcurring (N2 NodeImp l r) = (p1sModOcurring l) ++ (p1sModOcurring r)
p1sModOcurring _ = []

r2sModOcurring :: HyLoAST -> [RelId]
r2sModOcurring (LV NodeRel r) = [r]
r2sModOcurring (N1 NodeNeg f) = r2sModOcurring f
r2sModOcurring (N2 NodeDia r f) = (r2sModOcurring r) ++ (r2sModOcurring f)
r2sModOcurring (N2 NodeBox r f) = (r2sModOcurring r) ++ (r2sModOcurring f)
r2sModOcurring (N2 NodeAnd l r) = (r2sModOcurring l) ++ (r2sModOcurring r)
r2sModOcurring (N2 NodeOr l r) = (r2sModOcurring l) ++ (r2sModOcurring r)
r2sModOcurring (N2 NodeImp l r) = (r2sModOcurring l) ++ (r2sModOcurring r)
r2sModOcurring _ = []



p1sSig :: [HyLoAST] -> [(String, Int)]
p1sSig = pXsSig p1sModOcurring 0

r2sSig :: [HyLoAST] -> [(String, Int)]
r2sSig = rXsSig r2sModOcurring 0

pXsSig :: (HyLoAST -> [PropId]) -> Int -> [HyLoAST] -> [(String, Int)]
pXsSig f arity = map (flip (,) arity . relP) . nub . concatMap f

rXsSig :: (HyLoAST -> [RelId]) -> Int -> [HyLoAST] -> [(String, Int)]
rXsSig f arity = map (flip (,) arity . relR) . nub . concatMap f

relP :: PropId -> String
relP p = 'p':p

relR :: RelId -> String
relR r = 'r':r




formulas :: [HyLoAST] -> String
formulas fs = concat [" list_of_special_formulae(conjectures, EML).\n",
                       concatMap (\f -> concat ["prop_formula(not(", formula f, ")).\n"]) fs,
                       " end_of_list."]


formula :: HyLoAST -> String
formula (L NodeTrue)     = "true"
formula (L NodeFalse)    = "false"
formula (LV NodeProp p)  = relP p
formula (LV NodeRel r)   = relR r
formula (N1 NodeNeg f)   = concat ["not(", formula f, ")"]
formula (N2 NodeDia r f) = concat ["dia(", formula r, ", ", formula f, ")"]
formula (N2 NodeBox r f) = concat ["box(", formula r, ", ", formula f, ")"]
formula (N2 NodeAnd l r) = concat ["and(", formula l, ", ", formula r, ")"]
formula (N2 NodeOr l r)  = concat ["or(", formula l, ", ", formula r, ")"]
formula (N2 NodeImp l r) = concat ["implies(", formula l, ", ", formula r, ")"]
formula unknown          = error $ "Unexpected formula: " ++ astToString unknown
