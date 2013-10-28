{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
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

-- parser produced by Happy Version 1.18.4

data HappyAbsSyn 
	= HappyTerminal ((HyLoToken, FilePos))
	| HappyErrorToken Int
	| HappyAbsSyn4 ([HyLoAST])
	| HappyAbsSyn6 (HyLoAST)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> ((HyLoToken, FilePos))
	-> HappyState ((HyLoToken, FilePos)) (HappyStk HappyAbsSyn -> [((HyLoToken, FilePos))] -> m HappyAbsSyn)
	-> [HappyState ((HyLoToken, FilePos)) (HappyStk HappyAbsSyn -> [((HyLoToken, FilePos))] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [((HyLoToken, FilePos))] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> ((HyLoToken, FilePos))
	-> HappyState ((HyLoToken, FilePos)) (HappyStk HappyAbsSyn -> [((HyLoToken, FilePos))] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState ((HyLoToken, FilePos)) (HappyStk HappyAbsSyn -> [((HyLoToken, FilePos))] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [((HyLoToken, FilePos))] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> ((HyLoToken, FilePos))
	-> HappyState ((HyLoToken, FilePos)) (HappyStk HappyAbsSyn -> [((HyLoToken, FilePos))] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState ((HyLoToken, FilePos)) (HappyStk HappyAbsSyn -> [((HyLoToken, FilePos))] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [((HyLoToken, FilePos))] -> (HappyIdentity) HappyAbsSyn)

action_0 (7) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (7) = happyShift action_2
action_1 _ = happyFail

action_2 (10) = happyShift action_6
action_2 (11) = happyShift action_7
action_2 (12) = happyShift action_8
action_2 (13) = happyShift action_9
action_2 (14) = happyShift action_10
action_2 (15) = happyShift action_11
action_2 (16) = happyShift action_12
action_2 (17) = happyShift action_13
action_2 (22) = happyShift action_14
action_2 (23) = happyShift action_15
action_2 (24) = happyShift action_16
action_2 (25) = happyShift action_17
action_2 (26) = happyShift action_18
action_2 (5) = happyGoto action_4
action_2 (6) = happyGoto action_5
action_2 _ = happyFail

action_3 (30) = happyAccept
action_3 _ = happyFail

action_4 (8) = happyShift action_35
action_4 _ = happyFail

action_5 (18) = happyShift action_30
action_5 (19) = happyShift action_31
action_5 (20) = happyShift action_32
action_5 (21) = happyShift action_33
action_5 (28) = happyShift action_34
action_5 _ = happyReduce_2

action_6 (13) = happyShift action_28
action_6 (14) = happyShift action_29
action_6 _ = happyFail

action_7 (13) = happyShift action_27
action_7 _ = happyFail

action_8 _ = happyReduce_20

action_9 (9) = happyShift action_26
action_9 _ = happyReduce_18

action_10 (9) = happyShift action_25
action_10 _ = happyReduce_19

action_11 _ = happyReduce_21

action_12 _ = happyReduce_22

action_13 (10) = happyShift action_6
action_13 (11) = happyShift action_7
action_13 (12) = happyShift action_8
action_13 (13) = happyShift action_9
action_13 (14) = happyShift action_10
action_13 (15) = happyShift action_11
action_13 (16) = happyShift action_12
action_13 (17) = happyShift action_13
action_13 (22) = happyShift action_14
action_13 (23) = happyShift action_15
action_13 (24) = happyShift action_16
action_13 (25) = happyShift action_17
action_13 (26) = happyShift action_18
action_13 (6) = happyGoto action_24
action_13 _ = happyFail

action_14 (10) = happyShift action_6
action_14 (11) = happyShift action_7
action_14 (12) = happyShift action_8
action_14 (13) = happyShift action_9
action_14 (14) = happyShift action_10
action_14 (15) = happyShift action_11
action_14 (16) = happyShift action_12
action_14 (17) = happyShift action_13
action_14 (22) = happyShift action_14
action_14 (23) = happyShift action_15
action_14 (24) = happyShift action_16
action_14 (25) = happyShift action_17
action_14 (26) = happyShift action_18
action_14 (6) = happyGoto action_23
action_14 _ = happyFail

action_15 (10) = happyShift action_6
action_15 (11) = happyShift action_7
action_15 (12) = happyShift action_8
action_15 (13) = happyShift action_9
action_15 (14) = happyShift action_10
action_15 (15) = happyShift action_11
action_15 (16) = happyShift action_12
action_15 (17) = happyShift action_13
action_15 (22) = happyShift action_14
action_15 (23) = happyShift action_15
action_15 (24) = happyShift action_16
action_15 (25) = happyShift action_17
action_15 (26) = happyShift action_18
action_15 (6) = happyGoto action_22
action_15 _ = happyFail

action_16 (10) = happyShift action_6
action_16 (11) = happyShift action_7
action_16 (12) = happyShift action_8
action_16 (13) = happyShift action_9
action_16 (14) = happyShift action_10
action_16 (15) = happyShift action_11
action_16 (16) = happyShift action_12
action_16 (17) = happyShift action_13
action_16 (22) = happyShift action_14
action_16 (23) = happyShift action_15
action_16 (24) = happyShift action_16
action_16 (25) = happyShift action_17
action_16 (26) = happyShift action_18
action_16 (6) = happyGoto action_21
action_16 _ = happyFail

action_17 (10) = happyShift action_6
action_17 (11) = happyShift action_7
action_17 (12) = happyShift action_8
action_17 (13) = happyShift action_9
action_17 (14) = happyShift action_10
action_17 (15) = happyShift action_11
action_17 (16) = happyShift action_12
action_17 (17) = happyShift action_13
action_17 (22) = happyShift action_14
action_17 (23) = happyShift action_15
action_17 (24) = happyShift action_16
action_17 (25) = happyShift action_17
action_17 (26) = happyShift action_18
action_17 (6) = happyGoto action_20
action_17 _ = happyFail

action_18 (10) = happyShift action_6
action_18 (11) = happyShift action_7
action_18 (12) = happyShift action_8
action_18 (13) = happyShift action_9
action_18 (14) = happyShift action_10
action_18 (15) = happyShift action_11
action_18 (16) = happyShift action_12
action_18 (17) = happyShift action_13
action_18 (22) = happyShift action_14
action_18 (23) = happyShift action_15
action_18 (24) = happyShift action_16
action_18 (25) = happyShift action_17
action_18 (26) = happyShift action_18
action_18 (6) = happyGoto action_19
action_18 _ = happyFail

action_19 (18) = happyShift action_30
action_19 (19) = happyShift action_31
action_19 (20) = happyShift action_32
action_19 (21) = happyShift action_33
action_19 (27) = happyShift action_46
action_19 _ = happyFail

action_20 _ = happyReduce_6

action_21 _ = happyReduce_7

action_22 _ = happyReduce_4

action_23 _ = happyReduce_5

action_24 _ = happyReduce_10

action_25 (10) = happyShift action_6
action_25 (11) = happyShift action_7
action_25 (12) = happyShift action_8
action_25 (13) = happyShift action_9
action_25 (14) = happyShift action_10
action_25 (15) = happyShift action_11
action_25 (16) = happyShift action_12
action_25 (17) = happyShift action_13
action_25 (22) = happyShift action_14
action_25 (23) = happyShift action_15
action_25 (24) = happyShift action_16
action_25 (25) = happyShift action_17
action_25 (26) = happyShift action_18
action_25 (6) = happyGoto action_45
action_25 _ = happyFail

action_26 (10) = happyShift action_6
action_26 (11) = happyShift action_7
action_26 (12) = happyShift action_8
action_26 (13) = happyShift action_9
action_26 (14) = happyShift action_10
action_26 (15) = happyShift action_11
action_26 (16) = happyShift action_12
action_26 (17) = happyShift action_13
action_26 (22) = happyShift action_14
action_26 (23) = happyShift action_15
action_26 (24) = happyShift action_16
action_26 (25) = happyShift action_17
action_26 (26) = happyShift action_18
action_26 (6) = happyGoto action_44
action_26 _ = happyFail

action_27 (29) = happyShift action_43
action_27 _ = happyFail

action_28 (10) = happyShift action_6
action_28 (11) = happyShift action_7
action_28 (12) = happyShift action_8
action_28 (13) = happyShift action_9
action_28 (14) = happyShift action_10
action_28 (15) = happyShift action_11
action_28 (16) = happyShift action_12
action_28 (17) = happyShift action_13
action_28 (22) = happyShift action_14
action_28 (23) = happyShift action_15
action_28 (24) = happyShift action_16
action_28 (25) = happyShift action_17
action_28 (26) = happyShift action_18
action_28 (6) = happyGoto action_42
action_28 _ = happyFail

action_29 (10) = happyShift action_6
action_29 (11) = happyShift action_7
action_29 (12) = happyShift action_8
action_29 (13) = happyShift action_9
action_29 (14) = happyShift action_10
action_29 (15) = happyShift action_11
action_29 (16) = happyShift action_12
action_29 (17) = happyShift action_13
action_29 (22) = happyShift action_14
action_29 (23) = happyShift action_15
action_29 (24) = happyShift action_16
action_29 (25) = happyShift action_17
action_29 (26) = happyShift action_18
action_29 (6) = happyGoto action_41
action_29 _ = happyFail

action_30 (10) = happyShift action_6
action_30 (11) = happyShift action_7
action_30 (12) = happyShift action_8
action_30 (13) = happyShift action_9
action_30 (14) = happyShift action_10
action_30 (15) = happyShift action_11
action_30 (16) = happyShift action_12
action_30 (17) = happyShift action_13
action_30 (22) = happyShift action_14
action_30 (23) = happyShift action_15
action_30 (24) = happyShift action_16
action_30 (25) = happyShift action_17
action_30 (26) = happyShift action_18
action_30 (6) = happyGoto action_40
action_30 _ = happyFail

action_31 (10) = happyShift action_6
action_31 (11) = happyShift action_7
action_31 (12) = happyShift action_8
action_31 (13) = happyShift action_9
action_31 (14) = happyShift action_10
action_31 (15) = happyShift action_11
action_31 (16) = happyShift action_12
action_31 (17) = happyShift action_13
action_31 (22) = happyShift action_14
action_31 (23) = happyShift action_15
action_31 (24) = happyShift action_16
action_31 (25) = happyShift action_17
action_31 (26) = happyShift action_18
action_31 (6) = happyGoto action_39
action_31 _ = happyFail

action_32 (10) = happyShift action_6
action_32 (11) = happyShift action_7
action_32 (12) = happyShift action_8
action_32 (13) = happyShift action_9
action_32 (14) = happyShift action_10
action_32 (15) = happyShift action_11
action_32 (16) = happyShift action_12
action_32 (17) = happyShift action_13
action_32 (22) = happyShift action_14
action_32 (23) = happyShift action_15
action_32 (24) = happyShift action_16
action_32 (25) = happyShift action_17
action_32 (26) = happyShift action_18
action_32 (6) = happyGoto action_38
action_32 _ = happyFail

action_33 (10) = happyShift action_6
action_33 (11) = happyShift action_7
action_33 (12) = happyShift action_8
action_33 (13) = happyShift action_9
action_33 (14) = happyShift action_10
action_33 (15) = happyShift action_11
action_33 (16) = happyShift action_12
action_33 (17) = happyShift action_13
action_33 (22) = happyShift action_14
action_33 (23) = happyShift action_15
action_33 (24) = happyShift action_16
action_33 (25) = happyShift action_17
action_33 (26) = happyShift action_18
action_33 (6) = happyGoto action_37
action_33 _ = happyFail

action_34 (10) = happyShift action_6
action_34 (11) = happyShift action_7
action_34 (12) = happyShift action_8
action_34 (13) = happyShift action_9
action_34 (14) = happyShift action_10
action_34 (15) = happyShift action_11
action_34 (16) = happyShift action_12
action_34 (17) = happyShift action_13
action_34 (22) = happyShift action_14
action_34 (23) = happyShift action_15
action_34 (24) = happyShift action_16
action_34 (25) = happyShift action_17
action_34 (26) = happyShift action_18
action_34 (5) = happyGoto action_36
action_34 (6) = happyGoto action_5
action_34 _ = happyFail

action_35 _ = happyReduce_1

action_36 _ = happyReduce_3

action_37 (18) = happyShift action_30
action_37 (19) = happyShift action_31
action_37 (20) = happyShift action_32
action_37 (21) = happyShift action_33
action_37 _ = happyReduce_9

action_38 (18) = happyShift action_30
action_38 (19) = happyShift action_31
action_38 (20) = happyShift action_32
action_38 _ = happyReduce_8

action_39 (18) = happyShift action_30
action_39 _ = happyReduce_12

action_40 _ = happyReduce_11

action_41 (18) = happyShift action_30
action_41 (19) = happyShift action_31
action_41 (20) = happyShift action_32
action_41 (21) = happyShift action_33
action_41 _ = happyReduce_16

action_42 (18) = happyShift action_30
action_42 (19) = happyShift action_31
action_42 (20) = happyShift action_32
action_42 (21) = happyShift action_33
action_42 _ = happyReduce_14

action_43 (10) = happyShift action_6
action_43 (11) = happyShift action_7
action_43 (12) = happyShift action_8
action_43 (13) = happyShift action_9
action_43 (14) = happyShift action_10
action_43 (15) = happyShift action_11
action_43 (16) = happyShift action_12
action_43 (17) = happyShift action_13
action_43 (22) = happyShift action_14
action_43 (23) = happyShift action_15
action_43 (24) = happyShift action_16
action_43 (25) = happyShift action_17
action_43 (26) = happyShift action_18
action_43 (6) = happyGoto action_47
action_43 _ = happyFail

action_44 _ = happyReduce_13

action_45 _ = happyReduce_15

action_46 _ = happyReduce_23

action_47 _ = happyReduce_17

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1:happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal ((TokenDia happy_var_1 , _)))
	 =  HappyAbsSyn6
		 (N2 NodeDia (LV NodeRel happy_var_1) happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal ((TokenBox happy_var_1 , _)))
	 =  HappyAbsSyn6
		 (N2 NodeBox (LV NodeRel happy_var_1) happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  6 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (N1 NodeUDia happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  6 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (N1 NodeUBox happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (N2 NodeDimp happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (N2 NodeImp happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  6 happyReduction_10
happyReduction_10 (HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (N1 NodeNeg happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  6 happyReduction_11
happyReduction_11 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (N2 NodeAnd happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  6 happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (N2 NodeOr happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  6 happyReduction_13
happyReduction_13 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal ((TokenNom happy_var_1 , _)))
	 =  HappyAbsSyn6
		 (N2 NodeAt (LV NodeNom happy_var_1) happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  6 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_3)
	(HappyTerminal ((TokenNom happy_var_2 , _)))
	_
	 =  HappyAbsSyn6
		 (N2 NodeAt (LV NodeNom happy_var_2) happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  6 happyReduction_15
happyReduction_15 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal ((TokenVar happy_var_1 , _)))
	 =  HappyAbsSyn6
		 (N2 NodeAt (LV NodeVar happy_var_1) happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  6 happyReduction_16
happyReduction_16 (HappyAbsSyn6  happy_var_3)
	(HappyTerminal ((TokenVar happy_var_2 , _)))
	_
	 =  HappyAbsSyn6
		 (N2 NodeAt (LV NodeVar happy_var_2) happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 4 6 happyReduction_17
happyReduction_17 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((TokenNom happy_var_2 , _))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (N2 NodeDown (LV NodeNom happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_1  6 happyReduction_18
happyReduction_18 (HappyTerminal ((TokenNom happy_var_1 , _)))
	 =  HappyAbsSyn6
		 (LV NodeNom happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  6 happyReduction_19
happyReduction_19 (HappyTerminal ((TokenVar happy_var_1 , _)))
	 =  HappyAbsSyn6
		 (LV NodeVar happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  6 happyReduction_20
happyReduction_20 (HappyTerminal ((TokenProp happy_var_1, _)))
	 =  HappyAbsSyn6
		 (LV NodeProp happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  6 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn6
		 (L NodeTrue
	)

happyReduce_22 = happySpecReduce_1  6 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn6
		 (L NodeFalse
	)

happyReduce_23 = happySpecReduce_3  6 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 30 30 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	(TokenBegin  , _) -> cont 7;
	(TokenEnd    , _) -> cont 8;
	(TokenAt     , _) -> cont 9;
	(TokenAt2    , _) -> cont 10;
	(TokenDown   , _) -> cont 11;
	(TokenProp happy_dollar_dollar, _) -> cont 12;
	(TokenNom happy_dollar_dollar , _) -> cont 13;
	(TokenVar happy_dollar_dollar , _) -> cont 14;
	(TokenTrue   , _) -> cont 15;
	(TokenFalse  , _) -> cont 16;
	(TokenNeg    , _) -> cont 17;
	(TokenAnd    , _) -> cont 18;
	(TokenOr     , _) -> cont 19;
	(TokenDimp   , _) -> cont 20;
	(TokenImp    , _) -> cont 21;
	(TokenBox happy_dollar_dollar , _) -> cont 22;
	(TokenDia happy_dollar_dollar , _) -> cont 23;
	(TokenUBox   , _) -> cont 24;
	(TokenUDia   , _) -> cont 25;
	(TokenOB     , _) -> cont 26;
	(TokenCB     , _) -> cont 27;
	(TokenSC     , _) -> cont 28;
	(TokenP     , _) -> cont 29;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [((HyLoToken, FilePos))] -> HappyIdentity a
happyError' = HappyIdentity . happyError

hyloAstBuilder tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [(HyLoToken, FilePos)] -> a
happyError ((_, fp):_) = error ("Parse error near line " ++ 
                                   (show $ line fp) ++ 
                                   ", col. " ++
                                   (show $ col fp))
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}








{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
