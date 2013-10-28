----------------------------------------------------
--                                                --
-- HyLoASTBuilder.y:                              -- 
-- Hybrid Logic Syntax, parser file for Happy     --
-- Builds an Abstract Syntax Tree for (a set of)  --
-- hybrid formulas                                --
--                                                --
----------------------------------------------------

{
{-
Copyright (C) HyLoRes 2002-2004
Carlos Areces - carlos@science.uva.nl - http://www.illc.uva.nl/~carlos
Juan Heguiabehere - juanh@science.uva.nl - http://www.illc.uva.nl/~juanh
Daniel Gorin - dgorin@dc.uba.ar

Language and Inference Technology Group (LIT)
Institute for Logic Language and Information (ILLC)
University of Amsterdam (UvA)
Nieuwe Achtergracht 166, 1018WV Amsterdam. The Netherlands
 
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
 
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
 
You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,
USA.
-} 

module Prepo.HyLoASTBuilder(hyloAstBuilder, hyloLexer)
 
where 

import Prepo.AST
import Prepo.HyLoAST
import Prepo.HyLoLexer

}
  
%name hyloAstBuilder
%tokentype { (HyLoToken, FilePos) }

%token 
             begin           { (TokenBegin  , _) }
             end             { (TokenEnd    , _) }
             at              { (TokenAt     , _) } 
             at2             { (TokenAt2    , _) } 
             down            { (TokenDown   , _) }
             prop            { (TokenProp $$, _) }
             nom             { (TokenNom $$ , _) }
             var             { (TokenVar $$ , _) }
             true            { (TokenTrue   , _) }
             false           { (TokenFalse  , _) }
             neg             { (TokenNeg    , _) }
             and             { (TokenAnd    , _) }
             or              { (TokenOr     , _) }
             dimp            { (TokenDimp   , _) }
             imp             { (TokenImp    , _) }
             box             { (TokenBox $$ , _) }
             dia             { (TokenDia $$ , _) }
             ubox            { (TokenUBox   , _) }
             udia            { (TokenUDia   , _) }
             '('             { (TokenOB     , _) }
             ')'             { (TokenCB     , _) }
             ';'             { (TokenSC     , _) }
             '.'             { (TokenP     , _) }
%right imp
%right dimp
%left or
%left and
%left box ubox dia udia neg
%nonassoc at down '.'

%%

Input :: { [HyLoAST] }
Input : 
  begin Formulas end               { $2 } 

Formulas :: { [HyLoAST] }
Formulas :
  Formula                          { [$1] }
| Formula ';' Formulas             { $1:$3 }


Formula :: { HyLoAST }  
Formula : 
  dia Formula                      { N2 NodeDia (LV NodeRel $1) $2 }
| box Formula                      { N2 NodeBox (LV NodeRel $1) $2 }
| udia Formula                     { N1 NodeUDia $2 }
| ubox Formula                     { N1 NodeUBox $2 }
| Formula dimp Formula             { N2 NodeDimp $1 $3 }
| Formula imp Formula              { N2 NodeImp $1 $3 }
| neg Formula                      { N1 NodeNeg $2 }
| Formula and Formula              { N2 NodeAnd $1 $3 }
| Formula or Formula               { N2 NodeOr $1 $3 }
| nom at Formula                   { N2 NodeAt (LV NodeNom $1) $3 }
| at2 nom Formula                  { N2 NodeAt (LV NodeNom $2) $3 }
| var at Formula                   { N2 NodeAt (LV NodeVar $1) $3 }
| at2 var Formula                  { N2 NodeAt (LV NodeVar $2) $3 }
| down nom '.' Formula             { N2 NodeDown (LV NodeNom $2) $4 }
| nom                              { LV NodeNom $1 }
| var                              { LV NodeVar $1 }
| prop                             { LV NodeProp $1 }
| true                             { L NodeTrue }
| false                            { L NodeFalse }
| '(' Formula ')'                  { $2 }

{
happyError :: [(HyLoToken, FilePos)] -> a
happyError ((_, fp):_) = error ("Parse error near line " ++ 
                                   (show $ line fp) ++ 
                                   ", col. " ++
                                   (show $ col fp))
}
     
