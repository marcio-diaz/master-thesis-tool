module Prepo.ASTRewrite(TermVar(TV),
                  TermMatcherM, TermMatcher, MultiTermMatcher,
                  varMatcher, leafVarMatcher, leafMatcher, unaryNodeMatcher, binaryNodeMatcher, simpleRegExpMatcher, firstMatch,
                  AtomicExp(..), ContextRegExpC(..), ContextRegExp, Context,
                  TermWriter, writeVar, writeLeafVar, writeLeaf, writeUnaryNode, writeBinaryNode, writeInContext,
                  ASTRewriteRule, rewrite, normalForm)
where
import Control.Monad
import Data.Maybe
import Data.Map(Map, (!))
import qualified Data.Map as Map

import Prepo.AST

-------------------------
-- Contexts            --
-------------------------

data AtomicExp a = Anything | OneOf [a] | NoneOf [a] deriving (Eq, Ord, Show)
data ContextRegExpC a = Exactly (AtomicExp a) | Star (AtomicExp a) deriving(Eq, Ord, Show)
type ContextRegExp a = [ContextRegExpC a]
type Context a = (ContextRegExp a, Int)

atomicExpMatches :: Eq a => AtomicExp  a -> a -> Bool
atomicExpMatches Anything _ = True
atomicExpMatches (OneOf xs) y = y `elem` xs
atomicExpMatches (NoneOf xs) y = not $ y `elem` xs

newtype TermVar = TV Char deriving (Eq, Ord, Show)

type VarAssignment a = Map TermVar (AST a)
type SkeletonAssignment a = Map (Context a) (AST a)
type Matching a = (SkeletonAssignment a, VarAssignment a)

tryExpandContextM :: (MonadPlus m, Ord a) => Context a -> AST a -> [m (Matching a)] -> [m (Matching a)]
tryExpandContextM context ast matchings = zipWith updateContext matchings (descendantsComplement ast)
    where updateContext matching adjustment = do (mSkels, mVars) <- matching
                                                 return  (Map.adjust adjustment context mSkels, mVars)

-------------------------
-- Matcher             --
-------------------------

type TermMatcherM m a = AST a -> m (Matching a)
type TermMatcher a = TermMatcherM Maybe a
type MultiTermMatcher a = TermMatcherM [] a


varMatcher :: MonadPlus m => TermVar -> TermMatcherM m a
varMatcher v ast = return (Map.empty, Map.singleton v ast)

leafVarMatcher :: (MonadPlus m, Eq a) => a -> TermVar -> TermMatcherM m a
leafVarMatcher t v ast
    | isLeaf ast && t == nodeType ast = return (Map.empty, Map.singleton v ast)
    | otherwise                       = mzero

leafMatcher :: (MonadPlus m, Eq a) => AST a -> TermMatcherM m a
leafMatcher leaf ast
    | not $ isLeaf leaf               = error "Leaf matcher expects a leaf to match"
    | leaf == ast                     = return (Map.empty, Map.empty)
    | otherwise                       = mzero

unaryNodeMatcher :: (MonadPlus m, Eq a) => a -> TermMatcherM m a -> TermMatcherM m a
unaryNodeMatcher t matcherForSon (N1 t' son) = if t == t' then matcherForSon son else mzero
unaryNodeMatcher _ _              _          = mzero

binaryNodeMatcher :: (MonadPlus m, Show a, Ord a) => a -> TermMatcherM m a -> TermMatcherM m a -> TermMatcherM m a
binaryNodeMatcher t matcherLeft matcherRight (N2 t' l r) = if t == t'
                                                               then do vl <- matcherLeft l
                                                                       vr <- matcherRight r
                                                                       merge vl vr
                                                                else mzero
binaryNodeMatcher _ _           _             _          = mzero

simpleRegExpMatcher :: (MonadPlus m, Ord a) => Context a -> TermMatcherM m a -> TermMatcherM m a
simpleRegExpMatcher c@(re, _) = simpleRegExpMatcher' re c

simpleRegExpMatcher' :: (MonadPlus m, Ord a) => ContextRegExp a -> Context a -> TermMatcherM m a -> TermMatcherM m a
simpleRegExpMatcher'  []              context matcher ast =
    do
        (s, a) <- matcher ast
        return (Map.insert context PH s, a)
simpleRegExpMatcher' ((Exactly t):ts) context matcher ast
    | atomicExpMatches t (nodeType ast) =
        let
          sonsMatched  = map (simpleRegExpMatcher' ts context matcher) (descendants ast)
        in 
          msum $ tryExpandContextM context ast sonsMatched
    | otherwise                         = mzero
simpleRegExpMatcher' ((Star t):ts)    context matcher ast
    | atomicExpMatches t (nodeType ast) = (simpleRegExpMatcher' ((Exactly t):(Star t):ts) context matcher ast) `mplus`
                                          (simpleRegExpMatcher' ts context matcher ast)
    | otherwise                         = simpleRegExpMatcher' ts context matcher ast


-- merge tries to merge two matchings. the important conditions are:
-- +) the set of skeleton variables must be disjoint
-- +) the term assigned to each term variable occurring in both assignments must match
merge :: (MonadPlus m, Ord a, Show a) => Matching a -> Matching a -> m (Matching a)
merge (s1, t1) (s2, t2)
    | not . Map.null $ commonSkelVars          = error ("ASTRewrite.merge: Same context patterns matched twice: " ++ (show $ Map.keys commonSkelVars))
    | all snd $ Map.toList intersectionMatches = return (s1 `Map.union` s2, t1 `Map.union` t2)
    | otherwise                                = mzero
    where intersectionMatches  = Map.intersectionWith (==) t1 t2
          commonSkelVars       = Map.intersection s1 s2

firstMatch :: MultiTermMatcher a -> TermMatcher a
firstMatch m = listToMaybe . m

-------------------------
-- Writer              --
-------------------------

type TermWriter a = Matching a -> AST a

writeVar :: TermVar -> TermWriter a
writeVar v (_, a) = a ! v

writeLeafVar :: Eq a => a -> TermVar -> TermWriter a
writeLeafVar t v (_, a) | t == t' = ast
    where ast@(LV t' _) = a ! v


writeLeaf :: a -> Maybe String -> TermWriter a
writeLeaf t Nothing  _ = L t
writeLeaf t (Just v) _ = LV t v

writeUnaryNode :: a -> TermWriter a -> TermWriter a
writeUnaryNode t w m = N1 t (w m)

writeBinaryNode :: a -> TermWriter a -> TermWriter a -> TermWriter a
writeBinaryNode t wl wr m = N2 t (wl m) (wr m)

writeInContext :: Ord a => Context a -> TermWriter a -> TermWriter a
writeInContext c w m@(s, _) = replacePlaceHolder (s ! c) (w m)

-------------------------
-- Rewriter            --
-------------------------


type ASTRewriteRule a = AST a -> Maybe (AST a)

rewrite :: TermMatcher a -> TermWriter a -> ASTRewriteRule a
rewrite match write ast 
    | isLeaf ast = rewriteFromRoot
    | otherwise  = msum $ rewriteFromRoot : tryReplacementsM ast (map rewrite' (descendants ast))
        where rewriteFromRoot = match ast >>= return . write
              rewrite' = rewrite match write

normalForm :: Eq a => [ASTRewriteRule a] -> AST a -> AST a
normalForm rules ast
    | isNothing ruleApplied = ast
    | otherwise             = normalForm rules $ fromJust ruleApplied
        where ruleApplied = msum $ filterId $ map ($ ast) rules
              filterId    = filter (/= Just ast)
