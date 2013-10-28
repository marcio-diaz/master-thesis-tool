module OFTranslation

where

import Control.Monad(MonadPlus, mzero)
import Data.Map(Map, (!))
import Data.List
import Data.Maybe
import qualified Data.Map as Map(empty, insert)

import Prepo.AST
import Prepo.HyLoAST


type VarId = Int
type NomId = String
type IndexId = Int
type PropId = String
type SVarId = String
type RelId = String
type SortId = String

data Sort = World | Index
          deriving(Eq, Show)

data Var = X VarId 
         | S SVarId
         deriving(Eq, Show, Ord)

data Term = V Var
          | N NomId
          | F RelId IndexId Term
          deriving(Eq, Show)
                  
data SFOL = Term := Term
          | P1 PropId Term
          | R2 RelId Term Term
          | Neg SFOL
          | SFOL :& SFOL
          | SFOL :| SFOL
          | SFOL :-> SFOL
          | SFOL :<-> SFOL
          | Exi Var Sort SFOL
          | All Var Sort SFOL
          deriving(Eq, Show)
                  
data FOL = Term :== Term
         | P PropId Term
         | R RelId Term Term
         | Not FOL
         | FOL :&& FOL
         | FOL :|| FOL
         | FOL :--> FOL
         | FOL :<--> FOL
         | E Var FOL
         | A Var FOL
          deriving(Eq, Show)

                  

showsort :: Sort -> String
showsort World = "w"
showsort Index = "l"

prettyPrintTerm :: Term -> String
prettyPrintTerm (V (X i)) = "X" ++ show i
prettyPrintTerm (V (S i)) = "S" ++ i
prettyPrintTerm (N i) = "N" ++ i
prettyPrintTerm (F r i t) = "R" ++ r ++ "(X" ++ show i ++ ", " ++ prettyPrintTerm t ++ ")" 

                          
needParenSFOL :: SFOL -> Bool
needParenSFOL (s := t) = False 
needParenSFOL (P1 i t) = False 
needParenSFOL (R2 i s t) = False 
needParenSFOL (Neg f) = False 
needParenSFOL _ = True


                          
needParenFOL :: FOL -> Bool
needParenFOL (s :== t) = False 
needParenFOL (P i t) = False 
needParenFOL (R i s t) = False 
needParenFOL (Not f) = False 
needParenFOL _ = True


prettyPrintSFOL' :: SFOL -> String
prettyPrintSFOL' f = if (needParenSFOL f)
                      then "(" ++ (prettyPrintSFOL f) ++ ")"
                      else prettyPrintSFOL f

prettyPrintSFOL :: SFOL -> String
prettyPrintSFOL (s := t) = prettyPrintTerm s ++ " == " ++ prettyPrintTerm t
prettyPrintSFOL (P1 i t) = "P" ++ i ++ "(" ++ prettyPrintTerm t ++ ")"
prettyPrintSFOL (R2 i s t) = "R" ++ show i ++ "(" ++ prettyPrintTerm s ++ "," ++ prettyPrintTerm t ++ ")"
prettyPrintSFOL (Neg f) = "not " ++ prettyPrintSFOL' f 
prettyPrintSFOL (f :& g) = prettyPrintSFOL' f ++ " && " ++ prettyPrintSFOL' g
prettyPrintSFOL (f :| g) = prettyPrintSFOL' f  ++ " || " ++ prettyPrintSFOL' g
prettyPrintSFOL (f :-> g) = prettyPrintSFOL' f ++ " -> " ++ prettyPrintSFOL' g 
prettyPrintSFOL (f :<-> g) = prettyPrintSFOL' f  ++ " <-> " ++ prettyPrintSFOL' g
prettyPrintSFOL (Exi v s f) = "Exi " ++ printVar v ++ ":" ++ showsort s ++ "." ++ prettyPrintSFOL' f 
prettyPrintSFOL (All v s f) = "All " ++ printVar v ++ ":" ++ showsort s ++ "." ++ prettyPrintSFOL' f


prettyPrintFOL' :: FOL -> String
prettyPrintFOL' f = if (needParenFOL f)
                    then "(" ++ (prettyPrintFOL f) ++ ")"
                    else prettyPrintFOL f

prettyPrintFOL :: FOL -> String
prettyPrintFOL (s :== t) = prettyPrintTerm s ++ " == " ++ prettyPrintTerm t
prettyPrintFOL (P i t) = "P" ++ i ++ "(" ++ prettyPrintTerm t ++ ")"
prettyPrintFOL (R i s t) = "R" ++ show i ++ "(" ++ prettyPrintTerm s ++ "," ++ prettyPrintTerm t ++ ")"
prettyPrintFOL (Not f) = "¬ " ++ prettyPrintFOL' f 
prettyPrintFOL (f :&& g) = prettyPrintFOL' f ++ " && " ++ prettyPrintFOL' g
prettyPrintFOL (f :|| g) = prettyPrintFOL' f  ++ " || " ++ prettyPrintFOL' g
prettyPrintFOL (f :--> g) = prettyPrintFOL' f ++ " -> " ++ prettyPrintFOL' g 
prettyPrintFOL (f :<--> g) = prettyPrintFOL' f  ++ " <-> " ++ prettyPrintFOL' g
prettyPrintFOL (E v f) = "Exi " ++ printVar v ++ "." ++ prettyPrintFOL' f 
prettyPrintFOL (A v f) = "All " ++ printVar v ++ "." ++ prettyPrintFOL' f

var :: VarId -> Term
var = V . X
                  
svar :: SVarId -> Term
svar = V . S

termsOccurring :: SFOL -> [Term]
termsOccurring (l := r) = [l, r]
termsOccurring (P1 _ t) = [t]
termsOccurring (R2 _ t1 t2) = [t1, t2]
termsOccurring (Neg f) = termsOccurring f
termsOccurring (f1 :& f2) = (termsOccurring f1) ++ (termsOccurring f2)
termsOccurring (f1 :| f2) = (termsOccurring f1) ++ (termsOccurring f2)
termsOccurring (f1 :-> f2) = (termsOccurring f1) ++ (termsOccurring f2)
termsOccurring (f1 :<-> f2) = (termsOccurring f1) ++ (termsOccurring f2)
termsOccurring (Exi _ _ f) = termsOccurring f
termsOccurring (All _ _ f) = termsOccurring f

nomsOccurring :: SFOL -> [NomId]
nomsOccurring (l := r) = (nomInTerm l) ++ (nomInTerm r)
nomsOccurring (P1 _ t) = nomInTerm t
nomsOccurring (R2 r t1 t2) = (nomInTerm t1) ++ (nomInTerm t2)
nomsOccurring (Neg f) = nomsOccurring f
nomsOccurring (f1 :& f2) = (nomsOccurring f1) ++ (nomsOccurring f2)
nomsOccurring (f1 :| f2) = (nomsOccurring f1) ++ (nomsOccurring f2)
nomsOccurring (f1 :-> f2) = (nomsOccurring f1) ++ (nomsOccurring f2)
nomsOccurring (f1 :<-> f2) = (nomsOccurring f1) ++ (nomsOccurring f2)
nomsOccurring (Exi _ _ f) = nomsOccurring f
nomsOccurring (All _ _ f) = nomsOccurring f

nomsOccurringFOL :: FOL -> [NomId]
nomsOccurringFOL (l :== r) = (nomInTerm l) ++ (nomInTerm r)
nomsOccurringFOL (P _ t) = nomInTerm t
nomsOccurringFOL (R r t1 t2) = (nomInTerm t1) ++ (nomInTerm t2)
nomsOccurringFOL (Not f) = nomsOccurringFOL f
nomsOccurringFOL (f1 :&& f2) = (nomsOccurringFOL f1) ++ (nomsOccurringFOL f2)
nomsOccurringFOL (f1 :|| f2) = (nomsOccurringFOL f1) ++ (nomsOccurringFOL f2)
nomsOccurringFOL (f1 :--> f2) = (nomsOccurringFOL f1) ++ (nomsOccurringFOL f2)
nomsOccurringFOL (f1 :<--> f2) = (nomsOccurringFOL f1) ++ (nomsOccurringFOL f2)
nomsOccurringFOL (E _ f) = nomsOccurringFOL f
nomsOccurringFOL (A _ f) = nomsOccurringFOL f


nomInTerm :: MonadPlus m => Term -> m NomId
nomInTerm (V _) = mzero
nomInTerm (N n) = return n
nomInTerm (F r i t) = nomInTerm t

p1sOccurring :: SFOL -> [PropId]
p1sOccurring (_ := _) = []
p1sOccurring (P1 i _) = [i]
p1sOccurring (R2 _ _ _) = []
p1sOccurring (Neg f) = p1sOccurring f
p1sOccurring (f1 :& f2) = (p1sOccurring f1) ++ (p1sOccurring f2)
p1sOccurring (f1 :| f2) = (p1sOccurring f1) ++ (p1sOccurring f2)
p1sOccurring (f1 :-> f2) = (p1sOccurring f1) ++ (p1sOccurring f2)
p1sOccurring (f1 :<-> f2) = (p1sOccurring f1) ++ (p1sOccurring f2)
p1sOccurring (Exi _ _ f) = p1sOccurring f
p1sOccurring (All _ _ f) = p1sOccurring f

r2sOccurring :: SFOL -> [RelId]
r2sOccurring (_ := _) = []
r2sOccurring (P1 _ _) = []
r2sOccurring (R2 i _ _) = [i]
r2sOccurring (Neg f) = r2sOccurring f
r2sOccurring (f1 :& f2) = (r2sOccurring f1) ++ (r2sOccurring f2)
r2sOccurring (f1 :| f2) = (r2sOccurring f1) ++ (r2sOccurring f2)
r2sOccurring (f1 :-> f2) = (r2sOccurring f1) ++ (r2sOccurring f2)
r2sOccurring (f1 :<-> f2) = (r2sOccurring f1) ++ (r2sOccurring f2)
r2sOccurring (Exi _ _ f) = r2sOccurring f
r2sOccurring (All _ _ f) = r2sOccurring f


p1sOccurringFOL :: FOL -> [PropId]
p1sOccurringFOL (_ :== _) = []
p1sOccurringFOL (P i _) = [i]
p1sOccurringFOL (R _ _ _) = []
p1sOccurringFOL (Not f) = p1sOccurringFOL f
p1sOccurringFOL (f1 :&& f2) = (p1sOccurringFOL f1) ++ (p1sOccurringFOL f2)
p1sOccurringFOL (f1 :|| f2) = (p1sOccurringFOL f1) ++ (p1sOccurringFOL f2)
p1sOccurringFOL (f1 :--> f2) = (p1sOccurringFOL f1) ++ (p1sOccurringFOL f2)
p1sOccurringFOL (f1 :<--> f2) = (p1sOccurringFOL f1) ++ (p1sOccurringFOL f2)
p1sOccurringFOL (E _ f) = p1sOccurringFOL f
p1sOccurringFOL (A _ f) = p1sOccurringFOL f

r2sOccurringFOL :: FOL -> [RelId]
r2sOccurringFOL (_ :== _) = []
r2sOccurringFOL (P _ _) = []
r2sOccurringFOL (R i _ _) = [i]
r2sOccurringFOL (Not f) = r2sOccurringFOL f
r2sOccurringFOL (f1 :&& f2) = (r2sOccurringFOL f1) ++ (r2sOccurringFOL f2)
r2sOccurringFOL (f1 :|| f2) = (r2sOccurringFOL f1) ++ (r2sOccurringFOL f2)
r2sOccurringFOL (f1 :--> f2) = (r2sOccurringFOL f1) ++ (r2sOccurringFOL f2)
r2sOccurringFOL (f1 :<--> f2) = (r2sOccurringFOL f1) ++ (r2sOccurringFOL f2)
r2sOccurringFOL (E _ f) = r2sOccurringFOL f
r2sOccurringFOL (A _ f) = r2sOccurringFOL f


replaceInTerm :: Var -> Term -> Term -> Term
replaceInTerm v t x@(V v') = if v == v' then t else x
replaceInTerm _ _ n@(N _)  = n
replaceInTerm v t (F _ _ t') = replaceInTerm v t t'

replace :: Var -> Term -> SFOL -> SFOL
replace v t (l := r)        = (replaceInTerm v t l) := (replaceInTerm v t r)
replace v t (P1 p t')       = P1 p (replaceInTerm v t t')
replace v t (R2 r t1 t2)    = R2 r (replaceInTerm v t t1) (replaceInTerm v t t2)
replace v t (Neg f)         = Neg $ replace v t f
replace v t (f1 :& f2)      = (replace v t f1 :& replace v t f2)
replace v t (f1 :| f2)      = (replace v t f1 :| replace v t f2)
replace v t (f1 :-> f2)     = (replace v t f1 :-> replace v t f2)
replace v t (f1 :<-> f2)    = (replace v t f1 :<-> replace v t f2)
replace v t (Exi x s f)       = Exi x s $ if v == x then f 
                                          else replace v t f
replace v t (All x s f)       = All x s $ if v == x then f 
                                          else replace v t f
                                             

incrVar :: Var -> Var
incrVar (X i) = (X (i+1))
incrVar s = error $ "incrVar on state var " ++ (printVar s)

getId :: Var -> Int
getId (X i) = i
getId (S i) = (read i)::Int


replaceNomBySVarTerm :: NomId -> Term -> Term
replaceNomBySVarTerm i (N j) = if i == j then (V (S i)) else (N j)
replaceNomBySVarTerm i (F r j t) = F r j (replaceNomBySVarTerm i t)
replaceNomBySVarTerm _ t = t

replaceNomBySVars :: NomId -> SFOL -> SFOL
replaceNomBySVars i (s := t) = (replaceNomBySVarTerm i s) := (replaceNomBySVarTerm i t)
replaceNomBySVars i (P1 j t) = P1 j (replaceNomBySVarTerm i t)
replaceNomBySVars i (R2 j s t) = R2 j (replaceNomBySVarTerm i s) (replaceNomBySVarTerm i t)
replaceNomBySVars i (Neg f) = Neg (replaceNomBySVars i f)
replaceNomBySVars i (f :& g) = (replaceNomBySVars i f) :& (replaceNomBySVars i g)
replaceNomBySVars i (f :| g) = (replaceNomBySVars i f) :| (replaceNomBySVars i g)
replaceNomBySVars i (f :-> g) = (replaceNomBySVars i f) :-> (replaceNomBySVars i g)
replaceNomBySVars i (f :<-> g) = (replaceNomBySVars i f) :<-> (replaceNomBySVars i g)
replaceNomBySVars i (Exi j s f) = Exi j s (replaceNomBySVars i f)
replaceNomBySVars i (All j s f) = All j s (replaceNomBySVars i f)


nomToVar :: String -> HyLoAST -> HyLoAST
nomToVar i (LV NodeNom n) = if (i == n) then (LV NodeVar i) 
                            else (LV NodeNom n)
nomToVar i (N2 NodeBox r f) = N2 NodeBox r (nomToVar i f)
nomToVar i (N2 NodeDia r f) = N2 NodeDia r (nomToVar i f)
nomToVar i (N1 NodeNeg f) = N1 NodeNeg (nomToVar i f)
nomToVar i (N2 NodeAt n f) = N2 NodeAt (nomToVar i n) (nomToVar i f)
nomToVar i (N2 NodeDown n f) = N2 NodeDown n (nomToVar i f)
nomToVar i (N2 NodeAnd l r) = N2 NodeAnd (nomToVar i l) (nomToVar i r)
nomToVar i (N2 NodeOr l r) = N2 NodeOr (nomToVar i l) (nomToVar i r)
nomToVar i (N2 NodeImp l r) = N2 NodeImp (nomToVar i l) (nomToVar i r)
nomToVar i (N2 NodeDimp l r) = N2 NodeDimp (nomToVar i l) (nomToVar i r)
nomToVar i t = t


ft :: Term -> Var -> HyLoAST -> SFOL
ft t x (L NodeTrue) = t := t
ft t x (L NodeFalse) = Neg $ ft t x (L NodeTrue)
ft t x (LV NodeNom n) = t := N n
ft t x (LV NodeVar y) = t := svar y
ft t x (LV NodeProp p) = P1 p t
ft t x (N2 NodeBox (LV NodeRel r) f) = (Neg (P1 ("de"++r) t)) :-> (All (incrVar x) Index (ft (F r (getId (incrVar x)) t) (incrVar x) f))
ft t x (N2 NodeDia (LV NodeRel r) f) = (Neg (P1 ("de"++r) t)) :& (Exi (incrVar x) Index (ft (F r (getId (incrVar x)) t) (incrVar x) f))
ft t x (N1 NodeNeg f) = Neg $ ft t x f  
ft t x (N2 NodeAt (LV NodeNom v) f) = ft (N v) x f
ft t x (N2 NodeAt (LV NodeVar v) f) = ft (V (S v)) x f
ft t x (N2 NodeDown (LV NodeNom i) f) = (Exi (S i) World (((V (S i)) := t) :& (ft t x (nomToVar i f))))
ft t x (N2 NodeAnd l r) = (ft t x l) :& (ft t x r)                    
ft t x (N2 NodeOr l r) = (ft t x l) :| (ft t x r)                    
ft t x (N2 NodeImp l r) = (ft t x l) :-> (ft t x r)
ft t x (N2 NodeDimp l r) = (ft t x l) :<-> (ft t x r)                    
ft t x unknow = error $ "Unsexpected formula: '" ++ show unknow ++ "'" 
                ++ " t: " ++ show t ++ " x: " ++ show x


functionalTranslation :: HyLoAST -> SFOL
--functionalTranslation f = Exi (X 1) World (ft (V (X 1)) (X 1) (nnfhl f))
functionalTranslation f = ft (V (X 1)) (X 1) (nnfhl f)


freeVarsTerm :: Term -> [Var]
freeVarsTerm (F _ i t) = [(X i)] ++ freeVarsTerm t
freeVarsTerm (N n) = []
freeVarsTerm (V (X i)) = [(X i)]
freeVarsTerm (V (S i)) = [(S i)]


freeVars :: SFOL -> [Var]
freeVars (s := t) = freeVarsTerm s ++ freeVarsTerm t
freeVars (P1 _ t) = freeVarsTerm t
freeVars (R2 _ s t) = freeVarsTerm s ++ freeVarsTerm t
freeVars (Neg f) = freeVars f
freeVars (f :& g) = freeVars f ++ freeVars g
freeVars (f :| g) = freeVars f ++ freeVars g
freeVars (f :-> g) = freeVars f ++ freeVars g
freeVars (f :<-> g) = freeVars f ++ freeVars g
freeVars (Exi i s f) = delete i (freeVars f)
freeVars (All i s f) = delete i (freeVars f)

vars :: SFOL -> [Var]
vars (s := t) = freeVarsTerm s ++ freeVarsTerm t
vars (P1 _ t) = freeVarsTerm t
vars (R2 _ s t) = freeVarsTerm s ++ freeVarsTerm t
vars (Neg f) = vars f
vars (f :& g) = vars f ++ vars g
vars (f :| g) = vars f ++ vars g
vars (f :-> g) = vars f ++ vars g
vars (f :<-> g) = vars f ++ vars g
vars (Exi i s f) = vars f
vars (All i s f) = vars f


isXVar :: Var -> Bool
isXVar (X i) = True
isXVar (S i) = False

isSVar :: Var -> Bool
isSVar (X i) = False
isSVar (S i) = True

filterXVar :: [Var] -> [Var]
filterXVar s = filter isXVar s

filterSVar :: [Var] -> [Var]
filterSVar s = filter isSVar s

newXVar :: [Var] -> Var
newXVar s = X (maximum (map getId (filterXVar s)) + 1)

newSVar :: [Var] -> Var
newSVar s = S (show (maximum (map getId (filterSVar s)) + 1))

--eqVar :: Var -> Var -> Bool
--eqVar (X x) (X y) = (x == y)
--eqVar (S x) (S y) = (x == y)
--eqVar _ _ = False

isFreeIn :: Var -> SFOL -> Bool
isFreeIn v f = not (isNothing $ find (== v) (freeVars f))

substInTerm :: Var -> Var -> Term -> Term
substInTerm x y (V z) = if (z == x) then (V y) else (V z)
substInTerm x y (F r i t) = if ((X i) == x)
                            then (F r (getId y) (substInTerm x y t))
                            else (F r i (substInTerm x y t))
substInTerm _ _ t = t 

subst :: Var -> Var -> SFOL -> SFOL
subst x y (s := t) = substInTerm x y s := substInTerm x y t
subst x y (P1 i t) = P1 i (substInTerm x y t)
subst x y (R2 i s t) = R2 i (substInTerm x y s) (substInTerm x y t)
subst x y (Neg f) = Neg (subst x y f)
subst x y (f :& g) = (subst x y f) :& (subst x y g)
subst x y (f :| g) = (subst x y f) :| (subst x y g)
subst x y (f :-> g) = (subst x y f) :-> (subst x y g)
subst x y (f :<-> g) = (subst x y f) :<-> (subst x y g)
subst x y (Exi i s f) = if (i == x) 
                        then (Exi i s f)
                        else (Exi i s (subst x y f))
subst x y (All i s f) = if (i == x) 
                        then (All i s f)
                        else (All i s (subst x y f))




-- esto da asco
pnf :: SFOL -> SFOL
pnf (Neg (All x s f)) = pnf_rec (Exi x s (Neg f))
pnf (Neg (Exi x s f)) = pnf_rec (All x s (Neg f))
pnf ((All x s f) :& g) = if (not $ isFreeIn x g) 
                         then pnf_rec (All x s ((pnf f) :& (pnf g)))
                         else pnf_rec (All z s ((pnf f') :& (pnf g')))
                           where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                 fv = (vars f) ++ (vars g)
                                 f' = subst x z f
                                 g' = g

pnf (g :& (All x s f)) = if (not $ isFreeIn x g) 
                         then pnf_rec (All x s ((pnf g) :& (pnf f)))
                         else pnf_rec (All z s ((pnf g') :& (pnf f')))
                           where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                 fv = (vars f) ++ (vars g)
                                 f' = subst x z f
                                 g' = g


pnf ((All x s f) :| g) = if (not $ isFreeIn x g) 
                         then pnf_rec (All x s ((pnf f) :| (pnf g)))
                         else pnf_rec (All z s ((pnf f') :| (pnf g')))
                           where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                 fv = (vars f) ++ (vars g)
                                 f' = subst x z f
                                 g' = g


pnf (g :| (All x s f)) = if (not $ isFreeIn x g) 
                         then pnf_rec (All x s ((pnf g) :| (pnf f)))
                         else pnf_rec (All z s ((pnf g') :| (pnf f')))
                           where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                 fv = (vars f) ++ (vars g)
                                 f' = subst x z f
                                 g' = g


pnf ((Exi x s f) :& g) = if (not $ isFreeIn x g) 
                         then pnf_rec (Exi x s ((pnf f) :& (pnf g)))
                         else pnf_rec (Exi z s ((pnf f') :& (pnf g')))
                           where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                 fv = (vars f) ++ (vars g)
                                 f' = subst x z f
                                 g' = g

pnf (g :& (Exi x s f)) = if (not $ isFreeIn x g) 
                         then pnf_rec (Exi x s ((pnf g) :& (pnf f)))
                         else pnf_rec (Exi z s ((pnf g') :& (pnf f')))
                           where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                 fv = (vars f) ++ (vars g)
                                 f' = subst x z f
                                 g' = g



pnf ((Exi x s f) :| g) = if (not $ isFreeIn x g) 
                         then pnf_rec (Exi x s ((pnf f) :| (pnf g)))
                         else pnf_rec (Exi z s ((pnf f') :| (pnf g')))
                           where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                 fv = (vars f) ++ (vars g)
                                 f' = subst x z f
                                 g' = g

pnf (g :| (Exi x s f)) = if (not $ isFreeIn x g) 
                         then pnf_rec (Exi x s ((pnf g) :| (pnf f)))
                         else pnf_rec (Exi z s ((pnf g') :| (pnf f')))
                           where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                 fv = (vars f) ++ (vars g)
                                 f' = subst x z f
                                 g' = g

pnf ((All x s f) :-> g) = if (not $ isFreeIn x g) 
                         then pnf_rec (Exi x s ((pnf f) :-> (pnf g)))
                         else pnf_rec (Exi z s ((pnf f') :-> (pnf g')))
                           where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                 fv = (vars f) ++ (vars g)
                                 f' = subst x z f
                                 g' = g

pnf ((Exi x s f) :-> g) = if (not $ isFreeIn x g) 
                         then pnf_rec (All x s ((pnf f) :-> (pnf g)))
                         else pnf_rec (All z s ((pnf f') :-> (pnf g')))
                           where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                 fv = (vars f) ++ (vars g)
                                 f' = subst x z f
                                 g' = g


pnf (f :-> (All x s g)) = if (not $ isFreeIn x f) 
                          then pnf_rec (All x s ((pnf f) :-> (pnf g)))
                          else pnf_rec (All z s ((pnf f') :-> (pnf g')))
                            where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                  fv = (vars f) ++ (vars g)
                                  f' = f
                                  g' = subst x z g


pnf (f :-> (Exi x s g)) = if (not $ isFreeIn x f) 
                          then pnf_rec (Exi x s ((pnf f) :-> (pnf g)))
                          else pnf_rec (Exi z s ((pnf f') :-> (pnf g')))
                            where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                  fv = (vars f) ++ (vars g)
                                  f' = f
                                  g' = subst x z g
pnf f = f
        
        
pnf_rec :: SFOL -> SFOL
pnf_rec (Neg f) = pnf $ Neg (pnf_rec f)
pnf_rec (f :& g) = pnf $ (pnf_rec f) :& (pnf_rec g)
pnf_rec (f :| g) = pnf $ (pnf_rec f) :| (pnf_rec g)
pnf_rec (f :-> g) = pnf $ (pnf_rec f) :-> (pnf_rec g)
pnf_rec (f :<-> g) = pnf $ (pnf_rec f) :<-> (pnf_rec g)
pnf_rec (Exi i s f) = pnf $ Exi i s (pnf_rec f)
pnf_rec (All i s f) = pnf $ All i s (pnf_rec f)
pnf_rec f = f


removeExists :: SFOL -> SFOL
removeExists (Exi i s f) = if (s == Index)
                          then removeExists f
                          else Exi i s (removeExists f)
removeExists (All i s f) = All i s (removeExists f)
removeExists f = f

saveExistVars :: SFOL -> [Var]
saveExistVars (Exi i s f) = if (s == Index)
                            then i:(saveExistVars f)
                            else saveExistVars f
saveExistVars (All i s f) = saveExistVars f                                
saveExistVars f = []

moveExists' :: SFOL -> [Var] -> SFOL
moveExists' f [] = f
moveExists' f (e:l) = moveExists' (Exi e Index f) l

moveExists :: SFOL -> SFOL
moveExists f = moveExists' (removeExists f) (saveExistVars f)


-- sacar los forAll

removeForAll :: SFOL -> SFOL
removeForAll (All i s f) = removeForAll f
removeForAll (Exi i s f) = Exi i s (removeForAll f)
removeForAll f = f

saveForAllVars :: SFOL -> [(Var, Sort)]
saveForAllVars (All i s f) = (i,s):(saveForAllVars f)
saveForAllVars (Exi i s f) = saveForAllVars f   
saveForAllVars f = []

moveForAllOut' :: SFOL -> [(Var, Sort)] -> SFOL
moveForAllOut' f [] = f
moveForAllOut' f ((v,s):l) = moveForAllOut' (All v s f) l

moveForAllOut :: SFOL -> SFOL
moveForAllOut f = moveForAllOut' (removeForAll f) (saveForAllVars f)


-- meter los forAll


moveForAllIn' :: SFOL -> [(Var, Sort)] -> SFOL
moveForAllIn' f [] = f
moveForAllIn' (Exi i s f) xs = Exi i s (moveForAllIn' f xs)
moveForAllIn' f ((v,s):l) = moveForAllIn' (All v s f) l

moveForAllIn :: SFOL -> SFOL
moveForAllIn f = moveForAllIn' (removeForAll f) 
                  (saveForAllVars f)
------------------------


removeSorts :: SFOL -> FOL
removeSorts (s := t) = s :== t
removeSorts (P1 i t) = P i t
removeSorts (R2 r s t) = R r s t
removeSorts (Neg f) = Not (removeSorts f)
removeSorts (f :& g) = (removeSorts f) :&& (removeSorts g)
removeSorts (f :| g) = (removeSorts f) :|| (removeSorts g)
removeSorts (f :-> g) = (removeSorts f) :--> (removeSorts g)
removeSorts (f :<-> g) = (removeSorts f) :<--> (removeSorts g)
removeSorts (Exi i s f) = E i (removeSorts f)
removeSorts (All i s f) = A i (removeSorts f)


-----------------------------------------------------------------------
-- New Prenex

removeImp' :: SFOL -> SFOL
removeImp' (f :-> g) =  (Neg f) :| g
removeImp' (f :<-> g) = ((Neg f) :| g) :& (f :| (Neg g)) 
removeImp' f = f


removeImp :: SFOL -> SFOL
removeImp (Neg f) = Neg (removeImp f)
removeImp (f :& g) = (removeImp f) :& (removeImp g)
removeImp (f :| g) = (removeImp f) :| (removeImp g)
removeImp (f :-> g) = removeImp' $ (removeImp f) :-> (removeImp g)
removeImp (f :<-> g) = removeImp' $ (removeImp f) :<-> (removeImp g)
removeImp (Exi i s f) = Exi i s (removeImp f)
removeImp (All i s f) = All i s (removeImp f)
removeImp f = f

moveNegIn' :: SFOL -> SFOL
moveNegIn' (Neg (Neg f)) = moveNegIn $ f
moveNegIn' (Neg (Exi i s f)) = moveNegIn $ All i s (moveNegIn' (Neg f))
moveNegIn' (Neg (All i s f)) = moveNegIn $ Exi i s (moveNegIn' (Neg f))
moveNegIn' (Neg (f :& g)) = moveNegIn $ 
                            (moveNegIn' (Neg f)) :| (moveNegIn' (Neg g))
moveNegIn' (Neg (f :| g)) = moveNegIn $ 
                            (moveNegIn' (Neg f)) :& (moveNegIn' (Neg g))
moveNegIn' f = f


moveNegIn :: SFOL -> SFOL
moveNegIn (Neg f) = moveNegIn' $ Neg (moveNegIn f)
moveNegIn (f :& g) = moveNegIn' $ (moveNegIn f) :& (moveNegIn g)
moveNegIn (f :| g) = moveNegIn' $ (moveNegIn f) :| (moveNegIn g)
moveNegIn (f :-> g) = moveNegIn' $ (moveNegIn f) :-> (moveNegIn g)
moveNegIn (f :<-> g) = moveNegIn' $ (moveNegIn f) :<-> (moveNegIn g)
moveNegIn (Exi i s f) = moveNegIn' $ Exi i s (moveNegIn f)
moveNegIn (All i s f) = moveNegIn' $ All i s (moveNegIn f)
moveNegIn f = f




-----------------------------------------------------------------



moveCuantOut' :: SFOL -> SFOL
moveCuantOut' ((All x s f) :& g) = if (not $ isFreeIn x g) 
                          then moveCuantOut (All x s ((moveCuantOut' f) :& (moveCuantOut' g)))
                          else moveCuantOut (All z s ((moveCuantOut' f') :& (moveCuantOut' g')))
                            where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                  fv = (vars f) ++ (vars g)
                                  f' = subst x z f
                                  g' = g

moveCuantOut' (g :& (All x s f)) = if (not $ isFreeIn x g) 
                          then moveCuantOut (All x s ((moveCuantOut' g) :& (moveCuantOut' f)))
                          else moveCuantOut (All z s ((moveCuantOut' g') :& (moveCuantOut' f')))
                            where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                  fv = (vars f) ++ (vars g)
                                  f' = subst x z f
                                  g' = g


moveCuantOut' ((All x s f) :| g) = if (not $ isFreeIn x g) 
                          then moveCuantOut (All x s ((moveCuantOut' f) :| (moveCuantOut' g)))
                          else moveCuantOut (All z s ((moveCuantOut' f') :| (moveCuantOut' g')))
                            where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                  fv = (vars f) ++ (vars g)
                                  f' = subst x z f
                                  g' = g


moveCuantOut' (g :| (All x s f)) = if (not $ isFreeIn x g) 
                          then moveCuantOut (All x s ((moveCuantOut' g) :| (moveCuantOut' f)))
                          else moveCuantOut (All z s ((moveCuantOut' g') :| (moveCuantOut' f')))
                            where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                  fv = (vars f) ++ (vars g)
                                  f' = subst x z f
                                  g' = g


moveCuantOut' ((Exi x s f) :& g) = if (not $ isFreeIn x g) 
                          then moveCuantOut (Exi x s ((moveCuantOut' f) :& (moveCuantOut' g)))
                          else moveCuantOut (Exi z s ((moveCuantOut' f') :& (moveCuantOut' g')))
                            where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                  fv = (vars f) ++ (vars g)
                                  f' = subst x z f
                                  g' = g

moveCuantOut' (g :& (Exi x s f)) = if (not $ isFreeIn x g) 
                          then moveCuantOut (Exi x s ((moveCuantOut' g) :& (moveCuantOut' f)))
                          else moveCuantOut (Exi z s ((moveCuantOut' g') :& (moveCuantOut' f')))
                            where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                  fv = (vars f) ++ (vars g)
                                  f' = subst x z f
                                  g' = g



moveCuantOut' ((Exi x s f) :| g) = if (not $ isFreeIn x g) 
                          then moveCuantOut (Exi x s ((moveCuantOut' f) :| (moveCuantOut' g)))
                          else moveCuantOut (Exi z s ((moveCuantOut' f') :| (moveCuantOut' g')))
                            where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                  fv = (vars f) ++ (vars g)
                                  f' = subst x z f
                                  g' = g

moveCuantOut' (g :| (Exi x s f)) = if (not $ isFreeIn x g) 
                          then moveCuantOut (Exi x s ((moveCuantOut' g) :| (moveCuantOut' f)))
                          else moveCuantOut (Exi z s ((moveCuantOut' g') :| (moveCuantOut' f')))
                            where z = if (isXVar x) then (newXVar fv) 
                                     else (newSVar fv)
                                  fv = (vars f) ++ (vars g)
                                  f' = subst x z f
                                  g' = g
moveCuantOut' f = f
        
        
moveCuantOut :: SFOL -> SFOL
moveCuantOut (Neg f) = moveCuantOut' $ Neg (moveCuantOut f)
moveCuantOut (f :& g) = moveCuantOut' $ (moveCuantOut f) :& (moveCuantOut g)
moveCuantOut (f :| g) = moveCuantOut' $ (moveCuantOut f) :| (moveCuantOut g)
moveCuantOut (f :-> g) = moveCuantOut' $ (moveCuantOut f) :-> (moveCuantOut g)
moveCuantOut (f :<-> g) = moveCuantOut' $ (moveCuantOut f) :<-> (moveCuantOut g)
moveCuantOut (Exi i s f) = moveCuantOut' $ Exi i s (moveCuantOut f)
moveCuantOut (All i s f) = moveCuantOut' $ All i s (moveCuantOut f)
moveCuantOut f = f




nnfhl' :: HyLoAST -> HyLoAST
nnfhl' (N1 NodeNeg (N2 NodeDia r f))  = nnfhl $ N2 NodeBox r (nnfhl' (N1 NodeNeg f))
nnfhl' (N1 NodeNeg (N2 NodeBox r f))  = nnfhl $ N2 NodeDia r (nnfhl' (N1 NodeNeg f))
nnfhl' (N1 NodeNeg (N1 NodeNeg f))    = nnfhl $ f
nnfhl' (N1 NodeNeg (N2 NodeAt s f))   = nnfhl $ N2 NodeAt s (nnfhl' (N1 NodeNeg f))
nnfhl' (N1 NodeNeg (N2 NodeDown v f)) = nnfhl $ N2 NodeDown v (nnfhl' (N1 NodeNeg f))
nnfhl' (N1 NodeNeg (N2 NodeAnd l r))  = nnfhl $ N2 NodeOr (nnfhl' (N1 NodeNeg l)) (nnfhl' (N1 NodeNeg r))
nnfhl' (N1 NodeNeg (N2 NodeOr l r))   = nnfhl $ N2 NodeAnd (nnfhl' (N1 NodeNeg l)) (nnfhl' (N1 NodeNeg r))
nnfhl' (N1 NodeNeg (N2 NodeImp l r))  = nnfhl $ N2 NodeAnd (nnfhl' l) (nnfhl' (N1 NodeNeg r))
nnfhl' (N1 NodeNeg (N2 NodeDimp l r)) = nnfhl $ N2 NodeOr (nnfhl' (N1 NodeNeg (N2 NodeImp l r))) (nnfhl' (N2 NodeImp r l))
nnfhl' t = t


nnfhl :: HyLoAST -> HyLoAST
nnfhl (N2 NodeDia r f)  = nnfhl' $ N2 NodeDia r (nnfhl f)
nnfhl (N2 NodeBox r f)  = nnfhl' $ N2 NodeBox r (nnfhl f)
nnfhl (N1 NodeNeg f)    = nnfhl' $ N1 NodeNeg (nnfhl f)
nnfhl (N2 NodeAt s f)   = nnfhl' $ N2 NodeAt s (nnfhl f)
nnfhl (N2 NodeDown v f) = nnfhl' $ N2 NodeDown v (nnfhl f)
nnfhl (N2 NodeAnd l r)  = nnfhl' $ N2 NodeAnd (nnfhl l) (nnfhl r)
nnfhl (N2 NodeOr l r)   = nnfhl' $ N2 NodeOr (nnfhl l) (nnfhl r)
nnfhl (N2 NodeImp l r)  = nnfhl' $ N2 NodeImp (nnfhl l) (nnfhl r)
nnfhl (N2 NodeDimp l r) = nnfhl' $ N2 NodeDimp (nnfhl l) (nnfhl r)
nnfhl t = t



printVar :: Var -> String
printVar (X i) = "X"++(show i)
printVar (S i) = "S"++i

-------------------------------------------------------------------------------
---- Main functions

pnfFuncTrans :: HyLoAST -> SFOL
pnfFuncTrans = moveCuantOut . moveNegIn . removeImp . functionalTranslation

removeImpTrans :: HyLoAST -> SFOL
removeImpTrans = removeImp . functionalTranslation

moveNegInTrans :: HyLoAST -> SFOL
moveNegInTrans = moveNegIn . removeImp . functionalTranslation


withSortsAllOut :: HyLoAST -> SFOL
withSortsAllOut = moveForAllOut .  pnfFuncTrans

withSortsAllIn :: HyLoAST -> SFOL
withSortsAllIn = moveForAllIn . pnfFuncTrans

withSorts :: HyLoAST -> SFOL
withSorts = moveExists . pnfFuncTrans

withoutSorts :: HyLoAST -> FOL
withoutSorts = removeSorts . withSorts

optFuncTrans :: HyLoAST -> FOL
optFuncTrans =  removeSorts . moveExists . pnfFuncTrans

eagerSortsRemTrans :: HyLoAST -> FOL
eagerSortsRemTrans = removeSorts . moveNegInTrans



joinForms :: [HyLoAST] -> HyLoAST
joinForms [f] = f
joinForms (f:fs) = N2 NodeAnd f (joinForms fs)

----------------------------------------------------------------------------

unsorted :: SFOL -> FOL
unsorted (s := t) = s :== t
unsorted (R2 i s t) = R i s t
unsorted (P1 i t) = P (i++"0") t
unsorted (Neg f) = Not (unsorted f)
unsorted (f :& g) = (unsorted f) :&& (unsorted g)
unsorted (f :| g) = (unsorted f) :|| (unsorted g)
unsorted (f :-> g) = (unsorted f) :--> (unsorted g)
unsorted (f :<-> g) = (unsorted f) :<--> (unsorted g)
unsorted (Exi v World f) = E v ((P "0" (V v)) :&& (unsorted f))
unsorted (Exi v Index f) = E v ((P "1" (V v)) :&& (unsorted f))
unsorted (All v World f) = A v ((P "0" (V v)) :--> (unsorted f))
unsorted (All v Index f) = A v ((P "1" (V v)) :--> (unsorted f))

unsortedFuncTrans :: HyLoAST -> FOL
unsortedFuncTrans f = unsorted (functionalTranslation f)

funcTransUnsorted :: HyLoAST -> FOL
funcTransUnsorted f = unsorted (functionalTranslation f)

pnfFuncTransUnsorted :: HyLoAST -> FOL
pnfFuncTransUnsorted f = unsorted (pnfFuncTrans f)

withSortsUnsorted :: HyLoAST -> FOL
withSortsUnsorted f = unsorted (withSorts f)
