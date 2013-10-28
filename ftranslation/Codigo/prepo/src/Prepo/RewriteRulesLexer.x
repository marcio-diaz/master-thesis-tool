{
module Prepo.RewriteRulesLexer (RewriteRulesToken(..), FilePos(..), rewriteRulesLexer) where

import Prepo.HyLoAST(HyLoNodeType(..))
import Prepo.ASTRewrite(TermVar(..))

}

%wrapper "posn"

$digit = [0-9]                  -- digits
$alpha = [a-zA-Z]               -- alphabetic characters
$alphaNum = [$alpha$digit]
$var = [N-Z]

@relName = [Rr]$digit+

hyloTokens :-
  $white+                               ;       -- strip whitspaces
  \%.*                                  ;       -- strip remarks
  \#.*                                  ;       -- strip remarks

  true                           { discardValue $ TokenNode NodeTrue }
  false                          { discardValue $ TokenNode NodeFalse }
  p$digit+                       { storeValue   $ (TokenNodeVal NodeProp) . tail }
  p$var                          { storeValue   $ (TokenLeafVar NodeProp) . TV . last }
  n$digit+                       { storeValue   $ (TokenNodeVal NodeNom) . tail }
  n$var                          { storeValue   $ (TokenLeafVar NodeNom) . TV . last }
  x$digit+                       { storeValue   $ (TokenNodeVal NodeVar) . tail }
  x$var                          { storeValue   $ (TokenLeafVar NodeVar) . TV . last }
  r$digit+                       { storeValue   $ (TokenNodeVal NodeRel) . tail }
  r$var                          { storeValue   $ (TokenLeafVar NodeRel) . TV . last }
  $var                           { storeValue   $ TokenVar . TV . last}
  
  "<>"                           { discardValue $ TokenNode NodeDia }
  \<$white*@relName$white*\>     { storeValue   $ (TokenNodeVal NodeDia) . tail . removeSpaces . unenclose }
  "[]"                           { discardValue $ TokenNode NodeBox }
  \[$white*@relName$white*\]     { storeValue   $ (TokenNodeVal NodeBox) . tail . removeSpaces . unenclose }
  \@                             { discardValue $ TokenNode NodeAt }
  A                              { discardValue $ TokenNode NodeUBox }
  E                              { discardValue $ TokenNode NodeUDia }
  down                           { discardValue $ TokenNode NodeDown }
     
  [v\|]                          { discardValue $ TokenNode NodeOr }
  [&\^]                          { discardValue $ TokenNode NodeAnd }
  "-"                            { discardValue $ TokenNode NodeNeg }
  "<->"                          { discardValue $ TokenNode NodeDimp }
  "->"                           { discardValue $ TokenNode NodeImp }

  _$digit+                       { storeValue $ (TokenSubIndex) . read . tail }
  
  "("                            { discardValue TokenOB }
  ")"                            { discardValue TokenCB }
  "["                            { discardValue TokenOSB }
  "]"                            { discardValue TokenCSB }
  "{"                            { discardValue TokenOCB }
  "}"                            { discardValue TokenCCB }
  ","                            { discardValue TokenC }
  
  "."                            { discardValue TokenDot }
  "*"                            { discardValue TokenStar }
  "+"                            { discardValue TokenPlus }
  "~"                            { discardValue TokenTilde }
  
  \=+\>                          { discardValue TokenRewrite }  
  
  ";"                            { discardValue TokenSC }  
  ".include"                     { discardValue TokenInclude}
  \".+\"                         { storeValue $ TokenFileName . unenclose }
  
{
data RewriteRulesToken = TokenNode HyLoNodeType | TokenNodeVal HyLoNodeType String
                       | TokenVar TermVar | TokenLeafVar HyLoNodeType TermVar
                       | TokenSubIndex Int 
                       | TokenOB | TokenCB | TokenOSB | TokenCSB | TokenOCB | TokenCCB
                       | TokenDot | TokenStar | TokenPlus | TokenTilde
                       | TokenSC | TokenC | TokenRewrite
                       | TokenInclude | TokenFileName String
  deriving (Eq, Show)

data FilePos = FP{line :: Int, col :: Int} deriving (Eq, Show)
  
rewriteRulesLexer :: String -> [(RewriteRulesToken, FilePos)]
rewriteRulesLexer str = go (alexStartPos,'\n',[],str)
    where go inp@(apos,pos,_,str) = case alexScan inp 0 of
                                    AlexEOF                       -> []
                                    AlexError (_,_,_,_) -> error ("Unknown token")
                                    AlexSkip  inp' len            -> go inp'
                                    AlexToken inp' len act        -> act apos (take len str) : go inp'

makeFilePos (AlexPn _ l c) = FP{line = l, col = c}

storeValue     token alexPos v = (token v, makeFilePos alexPos)   -- for building tokens which need the parsed string
discardValue   token alexPos _ = (token, makeFilePos alexPos)     -- for building tokens which discard the parsed string

unenclose = init . tail
removeSpaces = filter (/= ' ')



}
