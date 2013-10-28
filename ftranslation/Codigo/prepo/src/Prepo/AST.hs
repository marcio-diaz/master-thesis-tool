module Prepo.AST


(AST(..), isLeaf, isUnary, isBinary, nodeType, 
 descendants, descendantsComplement, 
 tryReplacements, tryReplacementsM, replacePlaceHolder)

where

data AST a = PH                   -- "place holder", used in rewriting
           | L a                  -- leaf with no value
           | LV a String          -- leaf with associated string value
           | N1 a (AST a)         -- unary node
           | N2 a (AST a) (AST a) -- binary node
               deriving (Eq, Show)

isLeaf :: AST a -> Bool
isLeaf (L _)    = True
isLeaf (LV _ _) = True
isLeaf  _       = False

isUnary :: AST a -> Bool
isUnary (N1 _ _) = True
isUnary  _       = False

isBinary :: AST a -> Bool
isBinary (N2 _ _ _) = True
isBinary  _         = False

nodeType :: AST a -> a
nodeType (L t)      = t
nodeType (LV t _)   = t
nodeType (N1 t _)   = t
nodeType (N2 t _ _) = t

descendants :: AST a -> [AST a]
descendants (L _)            = []
descendants (LV _ _)         = []
descendants (N1 _ son)       = [son]
descendants (N2 _ sonL sonR) = [sonL, sonR]

{-
This can be seen as a sort of "complement" operation of
"descendants". Let x be an AST, then we have:
all (x ==) $ zipWith ($) (descendantsComplement x) (descendants x)
-}
descendantsComplement :: AST a -> [(AST a -> AST a)]
descendantsComplement (L _)            = []
descendantsComplement (LV _ _)         = []
descendantsComplement (N1 t _)         = [(N1 t)]
descendantsComplement (N2 t sonL sonR) = [(\newSonL -> N2 t newSonL sonR), (N2 t sonL)]

tryReplacements :: AST a -> [AST a] -> [AST a]
tryReplacements r@(L _)          []                 = [r]
tryReplacements r@(LV _ _)       []                 = [r]
tryReplacements (N1 t _)         [newSon]           = [(N1 t newSon)]
tryReplacements (N2 t sonL sonR) [newSonL, newSonR] = [(N2 t newSonL sonR), (N2 t sonL newSonR)]

tryReplacementsM :: (Monad m) => AST a -> [m (AST a)] -> [m (AST a)]
tryReplacementsM r@(L _)          []                   = [return r]
tryReplacementsM r@(LV _ _)       []                   = [return r]
tryReplacementsM (N1 t _)         [newSonM]            = [newSonM >>= return . (N1 t)]
tryReplacementsM (N2 t sonL sonR) [newSonLM, newSonRM] = [do{newSonL <- newSonLM; return (N2 t newSonL sonR)},
                                                          do{newSonR <- newSonRM; return (N2 t sonL newSonR)}]

replacePlaceHolder :: AST a -> AST a -> AST a
replacePlaceHolder PH            a = a
replacePlaceHolder leaf@(L _)    _ = leaf
replacePlaceHolder leaf@(LV _ _) _ = leaf
replacePlaceHolder (N1 t s)      a = N1 t (replacePlaceHolder s a)
replacePlaceHolder (N2 t s1 s2)  a = N2 t (replacePlaceHolder s1 a) (replacePlaceHolder s2 a)
