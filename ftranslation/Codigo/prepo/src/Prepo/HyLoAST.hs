module Prepo.HyLoAST

(HyLoNodeType(..), HyLoAST, astToString) 

where

import Prepo.AST

data HyLoNodeType = NodeAt   | NodeDown | NodeBox | NodeDia |
                    NodeUBox | NodeUDia | NodeTrue | NodeFalse|
                    NodeImp  | NodeDimp |
                    NodeNeg  | NodeAnd  | NodeOr  | 
                    NodeProp | NodeNom  | NodeVar | NodeRel 
          deriving (Eq, Ord)

instance Show HyLoNodeType where
    show NodeAt    = "@"
    show NodeDown  = "down"
    show NodeBox   = "[]"
    show NodeDia   = "<>"
    show NodeUBox  = "A"
    show NodeUDia  = "E"
    show NodeTrue  = "true"
    show NodeFalse = "false"
    show NodeImp   = "->"
    show NodeDimp  = "<->"
    show NodeNeg   = "-"
    show NodeAnd   = "^"
    show NodeOr    = "v"
    show NodeProp  = "prop"
    show NodeNom   = "nom" 
    show NodeVar   = "var"
    show NodeRel   = "rel"
          
type HyLoAST = AST HyLoNodeType

astToString :: HyLoAST -> String
astToString (L NodeTrue)      = "true"
astToString (L NodeFalse)     = "false"
astToString (LV NodeNom n)    = 'n':n
astToString (LV NodeVar v)    = 'x':v
astToString (LV NodeProp p)   = 'p':p
astToString (LV NodeRel r)    = 'r':r
astToString (N2 NodeDia r f)  = "<" ++ (astToString r) ++ ">" ++ astToString' NodeDia f
astToString (N2 NodeBox r f)  = "[" ++ (astToString r) ++ "]" ++ astToString' NodeBox f
astToString (N1 NodeUBox f)   = 'A':astToString' NodeUBox f
astToString (N1 NodeUDia f)   = 'E':astToString' NodeUDia f
astToString (N1 NodeNeg f)    = '-':astToString' NodeNeg f
astToString (N2 NodeAt s f)   = (astToString s) ++ ":" ++ astToString' NodeAt f
astToString (N2 NodeDown v f) = "down(" ++ (astToString v) ++ " " ++ (astToString' NodeDown f) ++ ")"
astToString (N2 NodeAnd l r)  = (astToString' NodeAnd l) ++ " ^ " ++ (astToString' NodeAnd r)
astToString (N2 NodeOr l r)   = (astToString' NodeOr l) ++ " v " ++ (astToString' NodeOr r)
astToString (N2 NodeImp l r)  = (astToString' NodeImp l) ++ " -> " ++ (astToString' NodeImp r)
astToString (N2 NodeDimp l r) = (astToString' NodeDimp l) ++ " <-> " ++ (astToString' NodeDimp r)

astToString' :: HyLoNodeType -> HyLoAST -> String
astToString' t a = if ( requiresParenthesis t a ) 
                       then ("(" ++ astToString a ++ ")") 
                       else astToString a


requiresParenthesis :: HyLoNodeType -> HyLoAST -> Bool
requiresParenthesis _        (L  _)            = False
requiresParenthesis _        (LV _ _)          = False
requiresParenthesis _        (N1 _ _)          = False
requiresParenthesis _        (N2 NodeDown _ _) = False
requiresParenthesis _        (N2 NodeDia _ _)  = False
requiresParenthesis _        (N2 NodeBox _ _)  = False
requiresParenthesis NodeAnd  (N2 NodeAnd _ _)  = False
requiresParenthesis NodeOr   (N2 NodeOr _ _)   = False
requiresParenthesis NodeAt   (N2 NodeAt _ _)   = False
requiresParenthesis NodeDown  _                = False
requiresParenthesis NodeImp   _                = True
requiresParenthesis NodeDimp (N2 NodeDimp _ _) = False
requiresParenthesis _        (N2 _ _ _)        = True



