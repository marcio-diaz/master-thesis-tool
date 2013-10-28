{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Prepo.RewriteRulesParser (rewriteRulesLexer, rewriteRulesParser, rewriteRulesFromString, rewriteRulesFromFile)

where

import Control.Monad
import Data.Maybe
import Data.List

import Prepo.AST
import Prepo.ASTRewrite
import Prepo.HyLoAST(HyLoNodeType(..))

import Prepo.RewriteRulesLexer

-- parser produced by Happy Version 1.18.4

data HappyAbsSyn 
	= HappyTerminal ((RewriteRulesToken, FilePos))
	| HappyErrorToken Int
	| HappyAbsSyn4 (([FilePath], [ASTRewriteRule HyLoNodeType]))
	| HappyAbsSyn5 ([FilePath])
	| HappyAbsSyn6 (FilePath)
	| HappyAbsSyn7 ([ASTRewriteRule HyLoNodeType])
	| HappyAbsSyn8 (ASTRewriteRule HyLoNodeType)
	| HappyAbsSyn9 (LeftPatternParseInfo)
	| HappyAbsSyn10 (())
	| HappyAbsSyn18 ((TermWriter HyLoNodeType, [Context HyLoNodeType]))
	| HappyAbsSyn19 (TermWriter HyLoNodeType)
	| HappyAbsSyn26 (Context HyLoNodeType)
	| HappyAbsSyn27 (ContextRegExp HyLoNodeType)
	| HappyAbsSyn29 (AtomicExp HyLoNodeType)
	| HappyAbsSyn30 (HyLoNodeType)
	| HappyAbsSyn31 ([HyLoNodeType])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> ((RewriteRulesToken, FilePos))
	-> HappyState ((RewriteRulesToken, FilePos)) (HappyStk HappyAbsSyn -> [((RewriteRulesToken, FilePos))] -> m HappyAbsSyn)
	-> [HappyState ((RewriteRulesToken, FilePos)) (HappyStk HappyAbsSyn -> [((RewriteRulesToken, FilePos))] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [((RewriteRulesToken, FilePos))] -> m HappyAbsSyn
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
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> ((RewriteRulesToken, FilePos))
	-> HappyState ((RewriteRulesToken, FilePos)) (HappyStk HappyAbsSyn -> [((RewriteRulesToken, FilePos))] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState ((RewriteRulesToken, FilePos)) (HappyStk HappyAbsSyn -> [((RewriteRulesToken, FilePos))] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [((RewriteRulesToken, FilePos))] -> (HappyIdentity) HappyAbsSyn)

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
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> ((RewriteRulesToken, FilePos))
	-> HappyState ((RewriteRulesToken, FilePos)) (HappyStk HappyAbsSyn -> [((RewriteRulesToken, FilePos))] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState ((RewriteRulesToken, FilePos)) (HappyStk HappyAbsSyn -> [((RewriteRulesToken, FilePos))] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [((RewriteRulesToken, FilePos))] -> (HappyIdentity) HappyAbsSyn)

action_0 (32) = happyShift action_14
action_0 (33) = happyShift action_15
action_0 (34) = happyShift action_16
action_0 (35) = happyShift action_17
action_0 (36) = happyShift action_18
action_0 (37) = happyShift action_19
action_0 (38) = happyShift action_20
action_0 (39) = happyShift action_21
action_0 (42) = happyShift action_22
action_0 (43) = happyShift action_23
action_0 (44) = happyShift action_24
action_0 (45) = happyShift action_25
action_0 (46) = happyShift action_26
action_0 (47) = happyShift action_27
action_0 (48) = happyShift action_28
action_0 (49) = happyShift action_29
action_0 (50) = happyShift action_30
action_0 (51) = happyShift action_31
action_0 (52) = happyShift action_32
action_0 (53) = happyShift action_33
action_0 (54) = happyShift action_34
action_0 (55) = happyShift action_35
action_0 (61) = happyShift action_36
action_0 (65) = happyShift action_37
action_0 (68) = happyShift action_38
action_0 (70) = happyShift action_42
action_0 (4) = happyGoto action_39
action_0 (5) = happyGoto action_40
action_0 (6) = happyGoto action_41
action_0 (7) = happyGoto action_2
action_0 (8) = happyGoto action_3
action_0 (9) = happyGoto action_4
action_0 (13) = happyGoto action_5
action_0 (14) = happyGoto action_6
action_0 (15) = happyGoto action_7
action_0 (16) = happyGoto action_8
action_0 (26) = happyGoto action_9
action_0 (27) = happyGoto action_10
action_0 (28) = happyGoto action_11
action_0 (29) = happyGoto action_12
action_0 (30) = happyGoto action_13
action_0 _ = happyFail

action_1 (32) = happyShift action_14
action_1 (33) = happyShift action_15
action_1 (34) = happyShift action_16
action_1 (35) = happyShift action_17
action_1 (36) = happyShift action_18
action_1 (37) = happyShift action_19
action_1 (38) = happyShift action_20
action_1 (39) = happyShift action_21
action_1 (42) = happyShift action_22
action_1 (43) = happyShift action_23
action_1 (44) = happyShift action_24
action_1 (45) = happyShift action_25
action_1 (46) = happyShift action_26
action_1 (47) = happyShift action_27
action_1 (48) = happyShift action_28
action_1 (49) = happyShift action_29
action_1 (50) = happyShift action_30
action_1 (51) = happyShift action_31
action_1 (52) = happyShift action_32
action_1 (53) = happyShift action_33
action_1 (54) = happyShift action_34
action_1 (55) = happyShift action_35
action_1 (61) = happyShift action_36
action_1 (65) = happyShift action_37
action_1 (68) = happyShift action_38
action_1 (7) = happyGoto action_2
action_1 (8) = happyGoto action_3
action_1 (9) = happyGoto action_4
action_1 (13) = happyGoto action_5
action_1 (14) = happyGoto action_6
action_1 (15) = happyGoto action_7
action_1 (16) = happyGoto action_8
action_1 (26) = happyGoto action_9
action_1 (27) = happyGoto action_10
action_1 (28) = happyGoto action_11
action_1 (29) = happyGoto action_12
action_1 (30) = happyGoto action_13
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (32) = happyShift action_14
action_3 (33) = happyShift action_15
action_3 (34) = happyShift action_16
action_3 (35) = happyShift action_17
action_3 (36) = happyShift action_18
action_3 (37) = happyShift action_19
action_3 (38) = happyShift action_20
action_3 (39) = happyShift action_21
action_3 (42) = happyShift action_22
action_3 (43) = happyShift action_23
action_3 (44) = happyShift action_24
action_3 (45) = happyShift action_25
action_3 (46) = happyShift action_26
action_3 (47) = happyShift action_27
action_3 (48) = happyShift action_28
action_3 (49) = happyShift action_29
action_3 (50) = happyShift action_30
action_3 (51) = happyShift action_31
action_3 (52) = happyShift action_32
action_3 (53) = happyShift action_33
action_3 (54) = happyShift action_34
action_3 (55) = happyShift action_35
action_3 (61) = happyShift action_36
action_3 (65) = happyShift action_37
action_3 (68) = happyShift action_38
action_3 (7) = happyGoto action_78
action_3 (8) = happyGoto action_3
action_3 (9) = happyGoto action_4
action_3 (13) = happyGoto action_5
action_3 (14) = happyGoto action_6
action_3 (15) = happyGoto action_7
action_3 (16) = happyGoto action_8
action_3 (26) = happyGoto action_9
action_3 (27) = happyGoto action_10
action_3 (28) = happyGoto action_11
action_3 (29) = happyGoto action_12
action_3 (30) = happyGoto action_13
action_3 _ = happyReduce_6

action_4 (69) = happyShift action_77
action_4 _ = happyFail

action_5 _ = happyReduce_11

action_6 _ = happyReduce_12

action_7 _ = happyReduce_13

action_8 _ = happyReduce_14

action_9 (59) = happyShift action_76
action_9 _ = happyFail

action_10 (56) = happyShift action_75
action_10 _ = happyReduce_81

action_11 (33) = happyShift action_49
action_11 (44) = happyShift action_50
action_11 (46) = happyShift action_51
action_11 (49) = happyShift action_52
action_11 (50) = happyShift action_53
action_11 (51) = happyShift action_54
action_11 (52) = happyShift action_55
action_11 (53) = happyShift action_56
action_11 (54) = happyShift action_57
action_11 (55) = happyShift action_58
action_11 (61) = happyShift action_36
action_11 (65) = happyShift action_37
action_11 (68) = happyShift action_38
action_11 (27) = happyGoto action_74
action_11 (28) = happyGoto action_11
action_11 (29) = happyGoto action_12
action_11 (30) = happyGoto action_13
action_11 _ = happyReduce_83

action_12 (66) = happyShift action_72
action_12 (67) = happyShift action_73
action_12 _ = happyReduce_85

action_13 _ = happyReduce_89

action_14 (57) = happyShift action_71
action_14 _ = happyFail

action_15 (57) = happyShift action_70
action_15 _ = happyReduce_96

action_16 _ = happyReduce_36

action_17 _ = happyReduce_37

action_18 _ = happyReduce_38

action_19 _ = happyReduce_39

action_20 _ = happyReduce_40

action_21 _ = happyReduce_41

action_22 _ = happyReduce_42

action_23 (57) = happyShift action_69
action_23 _ = happyFail

action_24 (57) = happyShift action_68
action_24 _ = happyReduce_92

action_25 (57) = happyShift action_67
action_25 _ = happyFail

action_26 (57) = happyShift action_66
action_26 _ = happyReduce_93

action_27 _ = happyReduce_19

action_28 _ = happyReduce_20

action_29 (57) = happyShift action_65
action_29 _ = happyReduce_101

action_30 (57) = happyShift action_64
action_30 _ = happyReduce_97

action_31 (57) = happyShift action_63
action_31 _ = happyReduce_98

action_32 (57) = happyShift action_62
action_32 _ = happyReduce_99

action_33 (57) = happyShift action_61
action_33 _ = happyReduce_100

action_34 (57) = happyShift action_60
action_34 _ = happyReduce_95

action_35 (57) = happyShift action_59
action_35 _ = happyReduce_94

action_36 (33) = happyShift action_49
action_36 (44) = happyShift action_50
action_36 (46) = happyShift action_51
action_36 (49) = happyShift action_52
action_36 (50) = happyShift action_53
action_36 (51) = happyShift action_54
action_36 (52) = happyShift action_55
action_36 (53) = happyShift action_56
action_36 (54) = happyShift action_57
action_36 (55) = happyShift action_58
action_36 (30) = happyGoto action_47
action_36 (31) = happyGoto action_48
action_36 _ = happyFail

action_37 _ = happyReduce_88

action_38 (61) = happyShift action_46
action_38 _ = happyFail

action_39 (72) = happyAccept
action_39 _ = happyFail

action_40 (32) = happyShift action_14
action_40 (33) = happyShift action_15
action_40 (34) = happyShift action_16
action_40 (35) = happyShift action_17
action_40 (36) = happyShift action_18
action_40 (37) = happyShift action_19
action_40 (38) = happyShift action_20
action_40 (39) = happyShift action_21
action_40 (42) = happyShift action_22
action_40 (43) = happyShift action_23
action_40 (44) = happyShift action_24
action_40 (45) = happyShift action_25
action_40 (46) = happyShift action_26
action_40 (47) = happyShift action_27
action_40 (48) = happyShift action_28
action_40 (49) = happyShift action_29
action_40 (50) = happyShift action_30
action_40 (51) = happyShift action_31
action_40 (52) = happyShift action_32
action_40 (53) = happyShift action_33
action_40 (54) = happyShift action_34
action_40 (55) = happyShift action_35
action_40 (61) = happyShift action_36
action_40 (65) = happyShift action_37
action_40 (68) = happyShift action_38
action_40 (7) = happyGoto action_45
action_40 (8) = happyGoto action_3
action_40 (9) = happyGoto action_4
action_40 (13) = happyGoto action_5
action_40 (14) = happyGoto action_6
action_40 (15) = happyGoto action_7
action_40 (16) = happyGoto action_8
action_40 (26) = happyGoto action_9
action_40 (27) = happyGoto action_10
action_40 (28) = happyGoto action_11
action_40 (29) = happyGoto action_12
action_40 (30) = happyGoto action_13
action_40 _ = happyFail

action_41 (70) = happyShift action_42
action_41 (5) = happyGoto action_44
action_41 (6) = happyGoto action_41
action_41 _ = happyReduce_3

action_42 (71) = happyShift action_43
action_42 _ = happyFail

action_43 _ = happyReduce_5

action_44 _ = happyReduce_4

action_45 _ = happyReduce_2

action_46 (33) = happyShift action_49
action_46 (44) = happyShift action_50
action_46 (46) = happyShift action_51
action_46 (49) = happyShift action_52
action_46 (50) = happyShift action_53
action_46 (51) = happyShift action_54
action_46 (52) = happyShift action_55
action_46 (53) = happyShift action_56
action_46 (54) = happyShift action_57
action_46 (55) = happyShift action_58
action_46 (30) = happyGoto action_47
action_46 (31) = happyGoto action_133
action_46 _ = happyFail

action_47 (33) = happyShift action_49
action_47 (44) = happyShift action_50
action_47 (46) = happyShift action_51
action_47 (49) = happyShift action_52
action_47 (50) = happyShift action_53
action_47 (51) = happyShift action_54
action_47 (52) = happyShift action_55
action_47 (53) = happyShift action_56
action_47 (54) = happyShift action_57
action_47 (55) = happyShift action_58
action_47 (30) = happyGoto action_47
action_47 (31) = happyGoto action_132
action_47 _ = happyReduce_102

action_48 (62) = happyShift action_131
action_48 _ = happyFail

action_49 _ = happyReduce_96

action_50 _ = happyReduce_92

action_51 _ = happyReduce_93

action_52 _ = happyReduce_101

action_53 _ = happyReduce_97

action_54 _ = happyReduce_98

action_55 _ = happyReduce_99

action_56 _ = happyReduce_100

action_57 _ = happyReduce_95

action_58 _ = happyReduce_94

action_59 (32) = happyShift action_14
action_59 (33) = happyShift action_15
action_59 (34) = happyShift action_16
action_59 (35) = happyShift action_17
action_59 (36) = happyShift action_18
action_59 (37) = happyShift action_19
action_59 (38) = happyShift action_20
action_59 (39) = happyShift action_21
action_59 (42) = happyShift action_22
action_59 (43) = happyShift action_23
action_59 (44) = happyShift action_24
action_59 (45) = happyShift action_25
action_59 (46) = happyShift action_26
action_59 (47) = happyShift action_27
action_59 (48) = happyShift action_28
action_59 (49) = happyShift action_29
action_59 (50) = happyShift action_30
action_59 (51) = happyShift action_31
action_59 (52) = happyShift action_32
action_59 (53) = happyShift action_33
action_59 (54) = happyShift action_34
action_59 (55) = happyShift action_35
action_59 (61) = happyShift action_36
action_59 (65) = happyShift action_37
action_59 (68) = happyShift action_38
action_59 (9) = happyGoto action_130
action_59 (13) = happyGoto action_5
action_59 (14) = happyGoto action_6
action_59 (15) = happyGoto action_7
action_59 (16) = happyGoto action_8
action_59 (26) = happyGoto action_9
action_59 (27) = happyGoto action_10
action_59 (28) = happyGoto action_11
action_59 (29) = happyGoto action_12
action_59 (30) = happyGoto action_13
action_59 _ = happyFail

action_60 (32) = happyShift action_14
action_60 (33) = happyShift action_15
action_60 (34) = happyShift action_16
action_60 (35) = happyShift action_17
action_60 (36) = happyShift action_18
action_60 (37) = happyShift action_19
action_60 (38) = happyShift action_20
action_60 (39) = happyShift action_21
action_60 (42) = happyShift action_22
action_60 (43) = happyShift action_23
action_60 (44) = happyShift action_24
action_60 (45) = happyShift action_25
action_60 (46) = happyShift action_26
action_60 (47) = happyShift action_27
action_60 (48) = happyShift action_28
action_60 (49) = happyShift action_29
action_60 (50) = happyShift action_30
action_60 (51) = happyShift action_31
action_60 (52) = happyShift action_32
action_60 (53) = happyShift action_33
action_60 (54) = happyShift action_34
action_60 (55) = happyShift action_35
action_60 (61) = happyShift action_36
action_60 (65) = happyShift action_37
action_60 (68) = happyShift action_38
action_60 (9) = happyGoto action_129
action_60 (13) = happyGoto action_5
action_60 (14) = happyGoto action_6
action_60 (15) = happyGoto action_7
action_60 (16) = happyGoto action_8
action_60 (26) = happyGoto action_9
action_60 (27) = happyGoto action_10
action_60 (28) = happyGoto action_11
action_60 (29) = happyGoto action_12
action_60 (30) = happyGoto action_13
action_60 _ = happyFail

action_61 (32) = happyShift action_14
action_61 (33) = happyShift action_15
action_61 (34) = happyShift action_16
action_61 (35) = happyShift action_17
action_61 (36) = happyShift action_18
action_61 (37) = happyShift action_19
action_61 (38) = happyShift action_20
action_61 (39) = happyShift action_21
action_61 (42) = happyShift action_22
action_61 (43) = happyShift action_23
action_61 (44) = happyShift action_24
action_61 (45) = happyShift action_25
action_61 (46) = happyShift action_26
action_61 (47) = happyShift action_27
action_61 (48) = happyShift action_28
action_61 (49) = happyShift action_29
action_61 (50) = happyShift action_30
action_61 (51) = happyShift action_31
action_61 (52) = happyShift action_32
action_61 (53) = happyShift action_33
action_61 (54) = happyShift action_34
action_61 (55) = happyShift action_35
action_61 (61) = happyShift action_36
action_61 (65) = happyShift action_37
action_61 (68) = happyShift action_38
action_61 (9) = happyGoto action_128
action_61 (13) = happyGoto action_5
action_61 (14) = happyGoto action_6
action_61 (15) = happyGoto action_7
action_61 (16) = happyGoto action_8
action_61 (26) = happyGoto action_9
action_61 (27) = happyGoto action_10
action_61 (28) = happyGoto action_11
action_61 (29) = happyGoto action_12
action_61 (30) = happyGoto action_13
action_61 _ = happyFail

action_62 (32) = happyShift action_14
action_62 (33) = happyShift action_15
action_62 (34) = happyShift action_16
action_62 (35) = happyShift action_17
action_62 (36) = happyShift action_18
action_62 (37) = happyShift action_19
action_62 (38) = happyShift action_20
action_62 (39) = happyShift action_21
action_62 (42) = happyShift action_22
action_62 (43) = happyShift action_23
action_62 (44) = happyShift action_24
action_62 (45) = happyShift action_25
action_62 (46) = happyShift action_26
action_62 (47) = happyShift action_27
action_62 (48) = happyShift action_28
action_62 (49) = happyShift action_29
action_62 (50) = happyShift action_30
action_62 (51) = happyShift action_31
action_62 (52) = happyShift action_32
action_62 (53) = happyShift action_33
action_62 (54) = happyShift action_34
action_62 (55) = happyShift action_35
action_62 (61) = happyShift action_36
action_62 (65) = happyShift action_37
action_62 (68) = happyShift action_38
action_62 (9) = happyGoto action_127
action_62 (13) = happyGoto action_5
action_62 (14) = happyGoto action_6
action_62 (15) = happyGoto action_7
action_62 (16) = happyGoto action_8
action_62 (26) = happyGoto action_9
action_62 (27) = happyGoto action_10
action_62 (28) = happyGoto action_11
action_62 (29) = happyGoto action_12
action_62 (30) = happyGoto action_13
action_62 _ = happyFail

action_63 (32) = happyShift action_14
action_63 (33) = happyShift action_15
action_63 (34) = happyShift action_16
action_63 (35) = happyShift action_17
action_63 (36) = happyShift action_18
action_63 (37) = happyShift action_19
action_63 (38) = happyShift action_20
action_63 (39) = happyShift action_21
action_63 (42) = happyShift action_22
action_63 (43) = happyShift action_23
action_63 (44) = happyShift action_24
action_63 (45) = happyShift action_25
action_63 (46) = happyShift action_26
action_63 (47) = happyShift action_27
action_63 (48) = happyShift action_28
action_63 (49) = happyShift action_29
action_63 (50) = happyShift action_30
action_63 (51) = happyShift action_31
action_63 (52) = happyShift action_32
action_63 (53) = happyShift action_33
action_63 (54) = happyShift action_34
action_63 (55) = happyShift action_35
action_63 (61) = happyShift action_36
action_63 (65) = happyShift action_37
action_63 (68) = happyShift action_38
action_63 (9) = happyGoto action_126
action_63 (13) = happyGoto action_5
action_63 (14) = happyGoto action_6
action_63 (15) = happyGoto action_7
action_63 (16) = happyGoto action_8
action_63 (26) = happyGoto action_9
action_63 (27) = happyGoto action_10
action_63 (28) = happyGoto action_11
action_63 (29) = happyGoto action_12
action_63 (30) = happyGoto action_13
action_63 _ = happyFail

action_64 (32) = happyShift action_14
action_64 (33) = happyShift action_15
action_64 (34) = happyShift action_16
action_64 (35) = happyShift action_17
action_64 (36) = happyShift action_18
action_64 (37) = happyShift action_19
action_64 (38) = happyShift action_20
action_64 (39) = happyShift action_21
action_64 (42) = happyShift action_22
action_64 (43) = happyShift action_23
action_64 (44) = happyShift action_24
action_64 (45) = happyShift action_25
action_64 (46) = happyShift action_26
action_64 (47) = happyShift action_27
action_64 (48) = happyShift action_28
action_64 (49) = happyShift action_29
action_64 (50) = happyShift action_30
action_64 (51) = happyShift action_31
action_64 (52) = happyShift action_32
action_64 (53) = happyShift action_33
action_64 (54) = happyShift action_34
action_64 (55) = happyShift action_35
action_64 (61) = happyShift action_36
action_64 (65) = happyShift action_37
action_64 (68) = happyShift action_38
action_64 (9) = happyGoto action_125
action_64 (13) = happyGoto action_5
action_64 (14) = happyGoto action_6
action_64 (15) = happyGoto action_7
action_64 (16) = happyGoto action_8
action_64 (26) = happyGoto action_9
action_64 (27) = happyGoto action_10
action_64 (28) = happyGoto action_11
action_64 (29) = happyGoto action_12
action_64 (30) = happyGoto action_13
action_64 _ = happyFail

action_65 (32) = happyShift action_14
action_65 (33) = happyShift action_15
action_65 (34) = happyShift action_16
action_65 (35) = happyShift action_17
action_65 (36) = happyShift action_18
action_65 (37) = happyShift action_19
action_65 (38) = happyShift action_20
action_65 (39) = happyShift action_21
action_65 (42) = happyShift action_22
action_65 (43) = happyShift action_23
action_65 (44) = happyShift action_24
action_65 (45) = happyShift action_25
action_65 (46) = happyShift action_26
action_65 (47) = happyShift action_27
action_65 (48) = happyShift action_28
action_65 (49) = happyShift action_29
action_65 (50) = happyShift action_30
action_65 (51) = happyShift action_31
action_65 (52) = happyShift action_32
action_65 (53) = happyShift action_33
action_65 (54) = happyShift action_34
action_65 (55) = happyShift action_35
action_65 (61) = happyShift action_36
action_65 (65) = happyShift action_37
action_65 (68) = happyShift action_38
action_65 (9) = happyGoto action_124
action_65 (13) = happyGoto action_5
action_65 (14) = happyGoto action_6
action_65 (15) = happyGoto action_7
action_65 (16) = happyGoto action_8
action_65 (26) = happyGoto action_9
action_65 (27) = happyGoto action_10
action_65 (28) = happyGoto action_11
action_65 (29) = happyGoto action_12
action_65 (30) = happyGoto action_13
action_65 _ = happyFail

action_66 (40) = happyShift action_120
action_66 (41) = happyShift action_121
action_66 (42) = happyShift action_22
action_66 (16) = happyGoto action_118
action_66 (17) = happyGoto action_123
action_66 _ = happyFail

action_67 (32) = happyShift action_14
action_67 (33) = happyShift action_15
action_67 (34) = happyShift action_16
action_67 (35) = happyShift action_17
action_67 (36) = happyShift action_18
action_67 (37) = happyShift action_19
action_67 (38) = happyShift action_20
action_67 (39) = happyShift action_21
action_67 (42) = happyShift action_22
action_67 (43) = happyShift action_23
action_67 (44) = happyShift action_24
action_67 (45) = happyShift action_25
action_67 (46) = happyShift action_26
action_67 (47) = happyShift action_27
action_67 (48) = happyShift action_28
action_67 (49) = happyShift action_29
action_67 (50) = happyShift action_30
action_67 (51) = happyShift action_31
action_67 (52) = happyShift action_32
action_67 (53) = happyShift action_33
action_67 (54) = happyShift action_34
action_67 (55) = happyShift action_35
action_67 (61) = happyShift action_36
action_67 (65) = happyShift action_37
action_67 (68) = happyShift action_38
action_67 (9) = happyGoto action_122
action_67 (13) = happyGoto action_5
action_67 (14) = happyGoto action_6
action_67 (15) = happyGoto action_7
action_67 (16) = happyGoto action_8
action_67 (26) = happyGoto action_9
action_67 (27) = happyGoto action_10
action_67 (28) = happyGoto action_11
action_67 (29) = happyGoto action_12
action_67 (30) = happyGoto action_13
action_67 _ = happyFail

action_68 (40) = happyShift action_120
action_68 (41) = happyShift action_121
action_68 (42) = happyShift action_22
action_68 (16) = happyGoto action_118
action_68 (17) = happyGoto action_119
action_68 _ = happyFail

action_69 (32) = happyShift action_14
action_69 (33) = happyShift action_15
action_69 (34) = happyShift action_16
action_69 (35) = happyShift action_17
action_69 (36) = happyShift action_18
action_69 (37) = happyShift action_19
action_69 (38) = happyShift action_20
action_69 (39) = happyShift action_21
action_69 (42) = happyShift action_22
action_69 (43) = happyShift action_23
action_69 (44) = happyShift action_24
action_69 (45) = happyShift action_25
action_69 (46) = happyShift action_26
action_69 (47) = happyShift action_27
action_69 (48) = happyShift action_28
action_69 (49) = happyShift action_29
action_69 (50) = happyShift action_30
action_69 (51) = happyShift action_31
action_69 (52) = happyShift action_32
action_69 (53) = happyShift action_33
action_69 (54) = happyShift action_34
action_69 (55) = happyShift action_35
action_69 (61) = happyShift action_36
action_69 (65) = happyShift action_37
action_69 (68) = happyShift action_38
action_69 (9) = happyGoto action_117
action_69 (13) = happyGoto action_5
action_69 (14) = happyGoto action_6
action_69 (15) = happyGoto action_7
action_69 (16) = happyGoto action_8
action_69 (26) = happyGoto action_9
action_69 (27) = happyGoto action_10
action_69 (28) = happyGoto action_11
action_69 (29) = happyGoto action_12
action_69 (30) = happyGoto action_13
action_69 _ = happyFail

action_70 (36) = happyShift action_18
action_70 (37) = happyShift action_19
action_70 (38) = happyShift action_20
action_70 (39) = happyShift action_21
action_70 (42) = happyShift action_22
action_70 (12) = happyGoto action_113
action_70 (14) = happyGoto action_114
action_70 (15) = happyGoto action_115
action_70 (16) = happyGoto action_116
action_70 _ = happyFail

action_71 (36) = happyShift action_18
action_71 (37) = happyShift action_19
action_71 (38) = happyShift action_20
action_71 (39) = happyShift action_21
action_71 (42) = happyShift action_22
action_71 (11) = happyGoto action_109
action_71 (14) = happyGoto action_110
action_71 (15) = happyGoto action_111
action_71 (16) = happyGoto action_112
action_71 _ = happyFail

action_72 _ = happyReduce_87

action_73 _ = happyReduce_86

action_74 _ = happyReduce_84

action_75 _ = happyReduce_82

action_76 (65) = happyShift action_108
action_76 (10) = happyGoto action_107
action_76 _ = happyFail

action_77 (32) = happyShift action_85
action_77 (33) = happyShift action_86
action_77 (34) = happyShift action_87
action_77 (35) = happyShift action_88
action_77 (36) = happyShift action_89
action_77 (37) = happyShift action_90
action_77 (38) = happyShift action_91
action_77 (39) = happyShift action_92
action_77 (42) = happyShift action_93
action_77 (43) = happyShift action_94
action_77 (44) = happyShift action_95
action_77 (45) = happyShift action_96
action_77 (46) = happyShift action_97
action_77 (47) = happyShift action_98
action_77 (48) = happyShift action_99
action_77 (49) = happyShift action_100
action_77 (50) = happyShift action_101
action_77 (51) = happyShift action_102
action_77 (52) = happyShift action_103
action_77 (53) = happyShift action_104
action_77 (54) = happyShift action_105
action_77 (55) = happyShift action_106
action_77 (61) = happyShift action_36
action_77 (65) = happyShift action_37
action_77 (68) = happyShift action_38
action_77 (18) = happyGoto action_79
action_77 (21) = happyGoto action_80
action_77 (22) = happyGoto action_81
action_77 (23) = happyGoto action_82
action_77 (24) = happyGoto action_83
action_77 (26) = happyGoto action_84
action_77 (27) = happyGoto action_10
action_77 (28) = happyGoto action_11
action_77 (29) = happyGoto action_12
action_77 (30) = happyGoto action_13
action_77 _ = happyFail

action_78 _ = happyReduce_7

action_79 (63) = happyShift action_164
action_79 _ = happyFail

action_80 _ = happyReduce_48

action_81 _ = happyReduce_49

action_82 _ = happyReduce_50

action_83 _ = happyReduce_51

action_84 (59) = happyShift action_163
action_84 _ = happyFail

action_85 (57) = happyShift action_162
action_85 _ = happyFail

action_86 (57) = happyShift action_161
action_86 _ = happyReduce_96

action_87 _ = happyReduce_71

action_88 _ = happyReduce_72

action_89 _ = happyReduce_73

action_90 _ = happyReduce_74

action_91 _ = happyReduce_75

action_92 _ = happyReduce_76

action_93 _ = happyReduce_77

action_94 (57) = happyShift action_160
action_94 _ = happyFail

action_95 (57) = happyShift action_159
action_95 _ = happyReduce_92

action_96 (57) = happyShift action_158
action_96 _ = happyFail

action_97 (57) = happyShift action_157
action_97 _ = happyReduce_93

action_98 _ = happyReduce_56

action_99 _ = happyReduce_57

action_100 (57) = happyShift action_156
action_100 _ = happyReduce_101

action_101 (57) = happyShift action_155
action_101 _ = happyReduce_97

action_102 (57) = happyShift action_154
action_102 _ = happyReduce_98

action_103 (57) = happyShift action_153
action_103 _ = happyReduce_99

action_104 (57) = happyShift action_152
action_104 _ = happyReduce_100

action_105 (57) = happyShift action_151
action_105 _ = happyReduce_95

action_106 (57) = happyShift action_150
action_106 _ = happyReduce_94

action_107 (32) = happyShift action_14
action_107 (33) = happyShift action_15
action_107 (34) = happyShift action_16
action_107 (35) = happyShift action_17
action_107 (36) = happyShift action_18
action_107 (37) = happyShift action_19
action_107 (38) = happyShift action_20
action_107 (39) = happyShift action_21
action_107 (42) = happyShift action_22
action_107 (43) = happyShift action_23
action_107 (44) = happyShift action_24
action_107 (45) = happyShift action_25
action_107 (46) = happyShift action_26
action_107 (47) = happyShift action_27
action_107 (48) = happyShift action_28
action_107 (49) = happyShift action_29
action_107 (50) = happyShift action_30
action_107 (51) = happyShift action_31
action_107 (52) = happyShift action_32
action_107 (53) = happyShift action_33
action_107 (54) = happyShift action_34
action_107 (55) = happyShift action_35
action_107 (61) = happyShift action_36
action_107 (65) = happyShift action_37
action_107 (68) = happyShift action_38
action_107 (9) = happyGoto action_149
action_107 (13) = happyGoto action_5
action_107 (14) = happyGoto action_6
action_107 (15) = happyGoto action_7
action_107 (16) = happyGoto action_8
action_107 (26) = happyGoto action_9
action_107 (27) = happyGoto action_10
action_107 (28) = happyGoto action_11
action_107 (29) = happyGoto action_12
action_107 (30) = happyGoto action_13
action_107 _ = happyFail

action_108 (65) = happyShift action_148
action_108 _ = happyFail

action_109 (64) = happyShift action_147
action_109 _ = happyFail

action_110 _ = happyReduce_30

action_111 _ = happyReduce_31

action_112 _ = happyReduce_32

action_113 (64) = happyShift action_146
action_113 _ = happyFail

action_114 _ = happyReduce_35

action_115 _ = happyReduce_33

action_116 _ = happyReduce_34

action_117 (58) = happyShift action_145
action_117 _ = happyFail

action_118 _ = happyReduce_45

action_119 (64) = happyShift action_144
action_119 _ = happyFail

action_120 _ = happyReduce_43

action_121 _ = happyReduce_44

action_122 (58) = happyShift action_143
action_122 _ = happyFail

action_123 (64) = happyShift action_142
action_123 _ = happyFail

action_124 (58) = happyShift action_141
action_124 _ = happyFail

action_125 (64) = happyShift action_140
action_125 _ = happyFail

action_126 (64) = happyShift action_139
action_126 _ = happyFail

action_127 (64) = happyShift action_138
action_127 _ = happyFail

action_128 (64) = happyShift action_137
action_128 _ = happyFail

action_129 (58) = happyShift action_136
action_129 _ = happyFail

action_130 (58) = happyShift action_135
action_130 _ = happyFail

action_131 _ = happyReduce_90

action_132 _ = happyReduce_103

action_133 (62) = happyShift action_134
action_133 _ = happyFail

action_134 _ = happyReduce_91

action_135 _ = happyReduce_27

action_136 _ = happyReduce_26

action_137 (32) = happyShift action_14
action_137 (33) = happyShift action_15
action_137 (34) = happyShift action_16
action_137 (35) = happyShift action_17
action_137 (36) = happyShift action_18
action_137 (37) = happyShift action_19
action_137 (38) = happyShift action_20
action_137 (39) = happyShift action_21
action_137 (42) = happyShift action_22
action_137 (43) = happyShift action_23
action_137 (44) = happyShift action_24
action_137 (45) = happyShift action_25
action_137 (46) = happyShift action_26
action_137 (47) = happyShift action_27
action_137 (48) = happyShift action_28
action_137 (49) = happyShift action_29
action_137 (50) = happyShift action_30
action_137 (51) = happyShift action_31
action_137 (52) = happyShift action_32
action_137 (53) = happyShift action_33
action_137 (54) = happyShift action_34
action_137 (55) = happyShift action_35
action_137 (61) = happyShift action_36
action_137 (65) = happyShift action_37
action_137 (68) = happyShift action_38
action_137 (9) = happyGoto action_196
action_137 (13) = happyGoto action_5
action_137 (14) = happyGoto action_6
action_137 (15) = happyGoto action_7
action_137 (16) = happyGoto action_8
action_137 (26) = happyGoto action_9
action_137 (27) = happyGoto action_10
action_137 (28) = happyGoto action_11
action_137 (29) = happyGoto action_12
action_137 (30) = happyGoto action_13
action_137 _ = happyFail

action_138 (32) = happyShift action_14
action_138 (33) = happyShift action_15
action_138 (34) = happyShift action_16
action_138 (35) = happyShift action_17
action_138 (36) = happyShift action_18
action_138 (37) = happyShift action_19
action_138 (38) = happyShift action_20
action_138 (39) = happyShift action_21
action_138 (42) = happyShift action_22
action_138 (43) = happyShift action_23
action_138 (44) = happyShift action_24
action_138 (45) = happyShift action_25
action_138 (46) = happyShift action_26
action_138 (47) = happyShift action_27
action_138 (48) = happyShift action_28
action_138 (49) = happyShift action_29
action_138 (50) = happyShift action_30
action_138 (51) = happyShift action_31
action_138 (52) = happyShift action_32
action_138 (53) = happyShift action_33
action_138 (54) = happyShift action_34
action_138 (55) = happyShift action_35
action_138 (61) = happyShift action_36
action_138 (65) = happyShift action_37
action_138 (68) = happyShift action_38
action_138 (9) = happyGoto action_195
action_138 (13) = happyGoto action_5
action_138 (14) = happyGoto action_6
action_138 (15) = happyGoto action_7
action_138 (16) = happyGoto action_8
action_138 (26) = happyGoto action_9
action_138 (27) = happyGoto action_10
action_138 (28) = happyGoto action_11
action_138 (29) = happyGoto action_12
action_138 (30) = happyGoto action_13
action_138 _ = happyFail

action_139 (32) = happyShift action_14
action_139 (33) = happyShift action_15
action_139 (34) = happyShift action_16
action_139 (35) = happyShift action_17
action_139 (36) = happyShift action_18
action_139 (37) = happyShift action_19
action_139 (38) = happyShift action_20
action_139 (39) = happyShift action_21
action_139 (42) = happyShift action_22
action_139 (43) = happyShift action_23
action_139 (44) = happyShift action_24
action_139 (45) = happyShift action_25
action_139 (46) = happyShift action_26
action_139 (47) = happyShift action_27
action_139 (48) = happyShift action_28
action_139 (49) = happyShift action_29
action_139 (50) = happyShift action_30
action_139 (51) = happyShift action_31
action_139 (52) = happyShift action_32
action_139 (53) = happyShift action_33
action_139 (54) = happyShift action_34
action_139 (55) = happyShift action_35
action_139 (61) = happyShift action_36
action_139 (65) = happyShift action_37
action_139 (68) = happyShift action_38
action_139 (9) = happyGoto action_194
action_139 (13) = happyGoto action_5
action_139 (14) = happyGoto action_6
action_139 (15) = happyGoto action_7
action_139 (16) = happyGoto action_8
action_139 (26) = happyGoto action_9
action_139 (27) = happyGoto action_10
action_139 (28) = happyGoto action_11
action_139 (29) = happyGoto action_12
action_139 (30) = happyGoto action_13
action_139 _ = happyFail

action_140 (32) = happyShift action_14
action_140 (33) = happyShift action_15
action_140 (34) = happyShift action_16
action_140 (35) = happyShift action_17
action_140 (36) = happyShift action_18
action_140 (37) = happyShift action_19
action_140 (38) = happyShift action_20
action_140 (39) = happyShift action_21
action_140 (42) = happyShift action_22
action_140 (43) = happyShift action_23
action_140 (44) = happyShift action_24
action_140 (45) = happyShift action_25
action_140 (46) = happyShift action_26
action_140 (47) = happyShift action_27
action_140 (48) = happyShift action_28
action_140 (49) = happyShift action_29
action_140 (50) = happyShift action_30
action_140 (51) = happyShift action_31
action_140 (52) = happyShift action_32
action_140 (53) = happyShift action_33
action_140 (54) = happyShift action_34
action_140 (55) = happyShift action_35
action_140 (61) = happyShift action_36
action_140 (65) = happyShift action_37
action_140 (68) = happyShift action_38
action_140 (9) = happyGoto action_193
action_140 (13) = happyGoto action_5
action_140 (14) = happyGoto action_6
action_140 (15) = happyGoto action_7
action_140 (16) = happyGoto action_8
action_140 (26) = happyGoto action_9
action_140 (27) = happyGoto action_10
action_140 (28) = happyGoto action_11
action_140 (29) = happyGoto action_12
action_140 (30) = happyGoto action_13
action_140 _ = happyFail

action_141 _ = happyReduce_21

action_142 (32) = happyShift action_14
action_142 (33) = happyShift action_15
action_142 (34) = happyShift action_16
action_142 (35) = happyShift action_17
action_142 (36) = happyShift action_18
action_142 (37) = happyShift action_19
action_142 (38) = happyShift action_20
action_142 (39) = happyShift action_21
action_142 (42) = happyShift action_22
action_142 (43) = happyShift action_23
action_142 (44) = happyShift action_24
action_142 (45) = happyShift action_25
action_142 (46) = happyShift action_26
action_142 (47) = happyShift action_27
action_142 (48) = happyShift action_28
action_142 (49) = happyShift action_29
action_142 (50) = happyShift action_30
action_142 (51) = happyShift action_31
action_142 (52) = happyShift action_32
action_142 (53) = happyShift action_33
action_142 (54) = happyShift action_34
action_142 (55) = happyShift action_35
action_142 (61) = happyShift action_36
action_142 (65) = happyShift action_37
action_142 (68) = happyShift action_38
action_142 (9) = happyGoto action_192
action_142 (13) = happyGoto action_5
action_142 (14) = happyGoto action_6
action_142 (15) = happyGoto action_7
action_142 (16) = happyGoto action_8
action_142 (26) = happyGoto action_9
action_142 (27) = happyGoto action_10
action_142 (28) = happyGoto action_11
action_142 (29) = happyGoto action_12
action_142 (30) = happyGoto action_13
action_142 _ = happyFail

action_143 _ = happyReduce_17

action_144 (32) = happyShift action_14
action_144 (33) = happyShift action_15
action_144 (34) = happyShift action_16
action_144 (35) = happyShift action_17
action_144 (36) = happyShift action_18
action_144 (37) = happyShift action_19
action_144 (38) = happyShift action_20
action_144 (39) = happyShift action_21
action_144 (42) = happyShift action_22
action_144 (43) = happyShift action_23
action_144 (44) = happyShift action_24
action_144 (45) = happyShift action_25
action_144 (46) = happyShift action_26
action_144 (47) = happyShift action_27
action_144 (48) = happyShift action_28
action_144 (49) = happyShift action_29
action_144 (50) = happyShift action_30
action_144 (51) = happyShift action_31
action_144 (52) = happyShift action_32
action_144 (53) = happyShift action_33
action_144 (54) = happyShift action_34
action_144 (55) = happyShift action_35
action_144 (61) = happyShift action_36
action_144 (65) = happyShift action_37
action_144 (68) = happyShift action_38
action_144 (9) = happyGoto action_191
action_144 (13) = happyGoto action_5
action_144 (14) = happyGoto action_6
action_144 (15) = happyGoto action_7
action_144 (16) = happyGoto action_8
action_144 (26) = happyGoto action_9
action_144 (27) = happyGoto action_10
action_144 (28) = happyGoto action_11
action_144 (29) = happyGoto action_12
action_144 (30) = happyGoto action_13
action_144 _ = happyFail

action_145 _ = happyReduce_15

action_146 (32) = happyShift action_14
action_146 (33) = happyShift action_15
action_146 (34) = happyShift action_16
action_146 (35) = happyShift action_17
action_146 (36) = happyShift action_18
action_146 (37) = happyShift action_19
action_146 (38) = happyShift action_20
action_146 (39) = happyShift action_21
action_146 (42) = happyShift action_22
action_146 (43) = happyShift action_23
action_146 (44) = happyShift action_24
action_146 (45) = happyShift action_25
action_146 (46) = happyShift action_26
action_146 (47) = happyShift action_27
action_146 (48) = happyShift action_28
action_146 (49) = happyShift action_29
action_146 (50) = happyShift action_30
action_146 (51) = happyShift action_31
action_146 (52) = happyShift action_32
action_146 (53) = happyShift action_33
action_146 (54) = happyShift action_34
action_146 (55) = happyShift action_35
action_146 (61) = happyShift action_36
action_146 (65) = happyShift action_37
action_146 (68) = happyShift action_38
action_146 (9) = happyGoto action_190
action_146 (13) = happyGoto action_5
action_146 (14) = happyGoto action_6
action_146 (15) = happyGoto action_7
action_146 (16) = happyGoto action_8
action_146 (26) = happyGoto action_9
action_146 (27) = happyGoto action_10
action_146 (28) = happyGoto action_11
action_146 (29) = happyGoto action_12
action_146 (30) = happyGoto action_13
action_146 _ = happyFail

action_147 (32) = happyShift action_14
action_147 (33) = happyShift action_15
action_147 (34) = happyShift action_16
action_147 (35) = happyShift action_17
action_147 (36) = happyShift action_18
action_147 (37) = happyShift action_19
action_147 (38) = happyShift action_20
action_147 (39) = happyShift action_21
action_147 (42) = happyShift action_22
action_147 (43) = happyShift action_23
action_147 (44) = happyShift action_24
action_147 (45) = happyShift action_25
action_147 (46) = happyShift action_26
action_147 (47) = happyShift action_27
action_147 (48) = happyShift action_28
action_147 (49) = happyShift action_29
action_147 (50) = happyShift action_30
action_147 (51) = happyShift action_31
action_147 (52) = happyShift action_32
action_147 (53) = happyShift action_33
action_147 (54) = happyShift action_34
action_147 (55) = happyShift action_35
action_147 (61) = happyShift action_36
action_147 (65) = happyShift action_37
action_147 (68) = happyShift action_38
action_147 (9) = happyGoto action_189
action_147 (13) = happyGoto action_5
action_147 (14) = happyGoto action_6
action_147 (15) = happyGoto action_7
action_147 (16) = happyGoto action_8
action_147 (26) = happyGoto action_9
action_147 (27) = happyGoto action_10
action_147 (28) = happyGoto action_11
action_147 (29) = happyGoto action_12
action_147 (30) = happyGoto action_13
action_147 _ = happyFail

action_148 (65) = happyShift action_188
action_148 _ = happyFail

action_149 (65) = happyShift action_108
action_149 (10) = happyGoto action_187
action_149 _ = happyFail

action_150 (32) = happyShift action_85
action_150 (33) = happyShift action_86
action_150 (34) = happyShift action_87
action_150 (35) = happyShift action_88
action_150 (36) = happyShift action_89
action_150 (37) = happyShift action_90
action_150 (38) = happyShift action_91
action_150 (39) = happyShift action_92
action_150 (42) = happyShift action_93
action_150 (43) = happyShift action_94
action_150 (44) = happyShift action_95
action_150 (45) = happyShift action_96
action_150 (46) = happyShift action_97
action_150 (47) = happyShift action_98
action_150 (48) = happyShift action_99
action_150 (49) = happyShift action_100
action_150 (50) = happyShift action_101
action_150 (51) = happyShift action_102
action_150 (52) = happyShift action_103
action_150 (53) = happyShift action_104
action_150 (54) = happyShift action_105
action_150 (55) = happyShift action_106
action_150 (61) = happyShift action_36
action_150 (65) = happyShift action_37
action_150 (68) = happyShift action_38
action_150 (18) = happyGoto action_186
action_150 (21) = happyGoto action_80
action_150 (22) = happyGoto action_81
action_150 (23) = happyGoto action_82
action_150 (24) = happyGoto action_83
action_150 (26) = happyGoto action_84
action_150 (27) = happyGoto action_10
action_150 (28) = happyGoto action_11
action_150 (29) = happyGoto action_12
action_150 (30) = happyGoto action_13
action_150 _ = happyFail

action_151 (32) = happyShift action_85
action_151 (33) = happyShift action_86
action_151 (34) = happyShift action_87
action_151 (35) = happyShift action_88
action_151 (36) = happyShift action_89
action_151 (37) = happyShift action_90
action_151 (38) = happyShift action_91
action_151 (39) = happyShift action_92
action_151 (42) = happyShift action_93
action_151 (43) = happyShift action_94
action_151 (44) = happyShift action_95
action_151 (45) = happyShift action_96
action_151 (46) = happyShift action_97
action_151 (47) = happyShift action_98
action_151 (48) = happyShift action_99
action_151 (49) = happyShift action_100
action_151 (50) = happyShift action_101
action_151 (51) = happyShift action_102
action_151 (52) = happyShift action_103
action_151 (53) = happyShift action_104
action_151 (54) = happyShift action_105
action_151 (55) = happyShift action_106
action_151 (61) = happyShift action_36
action_151 (65) = happyShift action_37
action_151 (68) = happyShift action_38
action_151 (18) = happyGoto action_185
action_151 (21) = happyGoto action_80
action_151 (22) = happyGoto action_81
action_151 (23) = happyGoto action_82
action_151 (24) = happyGoto action_83
action_151 (26) = happyGoto action_84
action_151 (27) = happyGoto action_10
action_151 (28) = happyGoto action_11
action_151 (29) = happyGoto action_12
action_151 (30) = happyGoto action_13
action_151 _ = happyFail

action_152 (32) = happyShift action_85
action_152 (33) = happyShift action_86
action_152 (34) = happyShift action_87
action_152 (35) = happyShift action_88
action_152 (36) = happyShift action_89
action_152 (37) = happyShift action_90
action_152 (38) = happyShift action_91
action_152 (39) = happyShift action_92
action_152 (42) = happyShift action_93
action_152 (43) = happyShift action_94
action_152 (44) = happyShift action_95
action_152 (45) = happyShift action_96
action_152 (46) = happyShift action_97
action_152 (47) = happyShift action_98
action_152 (48) = happyShift action_99
action_152 (49) = happyShift action_100
action_152 (50) = happyShift action_101
action_152 (51) = happyShift action_102
action_152 (52) = happyShift action_103
action_152 (53) = happyShift action_104
action_152 (54) = happyShift action_105
action_152 (55) = happyShift action_106
action_152 (61) = happyShift action_36
action_152 (65) = happyShift action_37
action_152 (68) = happyShift action_38
action_152 (18) = happyGoto action_184
action_152 (21) = happyGoto action_80
action_152 (22) = happyGoto action_81
action_152 (23) = happyGoto action_82
action_152 (24) = happyGoto action_83
action_152 (26) = happyGoto action_84
action_152 (27) = happyGoto action_10
action_152 (28) = happyGoto action_11
action_152 (29) = happyGoto action_12
action_152 (30) = happyGoto action_13
action_152 _ = happyFail

action_153 (32) = happyShift action_85
action_153 (33) = happyShift action_86
action_153 (34) = happyShift action_87
action_153 (35) = happyShift action_88
action_153 (36) = happyShift action_89
action_153 (37) = happyShift action_90
action_153 (38) = happyShift action_91
action_153 (39) = happyShift action_92
action_153 (42) = happyShift action_93
action_153 (43) = happyShift action_94
action_153 (44) = happyShift action_95
action_153 (45) = happyShift action_96
action_153 (46) = happyShift action_97
action_153 (47) = happyShift action_98
action_153 (48) = happyShift action_99
action_153 (49) = happyShift action_100
action_153 (50) = happyShift action_101
action_153 (51) = happyShift action_102
action_153 (52) = happyShift action_103
action_153 (53) = happyShift action_104
action_153 (54) = happyShift action_105
action_153 (55) = happyShift action_106
action_153 (61) = happyShift action_36
action_153 (65) = happyShift action_37
action_153 (68) = happyShift action_38
action_153 (18) = happyGoto action_183
action_153 (21) = happyGoto action_80
action_153 (22) = happyGoto action_81
action_153 (23) = happyGoto action_82
action_153 (24) = happyGoto action_83
action_153 (26) = happyGoto action_84
action_153 (27) = happyGoto action_10
action_153 (28) = happyGoto action_11
action_153 (29) = happyGoto action_12
action_153 (30) = happyGoto action_13
action_153 _ = happyFail

action_154 (32) = happyShift action_85
action_154 (33) = happyShift action_86
action_154 (34) = happyShift action_87
action_154 (35) = happyShift action_88
action_154 (36) = happyShift action_89
action_154 (37) = happyShift action_90
action_154 (38) = happyShift action_91
action_154 (39) = happyShift action_92
action_154 (42) = happyShift action_93
action_154 (43) = happyShift action_94
action_154 (44) = happyShift action_95
action_154 (45) = happyShift action_96
action_154 (46) = happyShift action_97
action_154 (47) = happyShift action_98
action_154 (48) = happyShift action_99
action_154 (49) = happyShift action_100
action_154 (50) = happyShift action_101
action_154 (51) = happyShift action_102
action_154 (52) = happyShift action_103
action_154 (53) = happyShift action_104
action_154 (54) = happyShift action_105
action_154 (55) = happyShift action_106
action_154 (61) = happyShift action_36
action_154 (65) = happyShift action_37
action_154 (68) = happyShift action_38
action_154 (18) = happyGoto action_182
action_154 (21) = happyGoto action_80
action_154 (22) = happyGoto action_81
action_154 (23) = happyGoto action_82
action_154 (24) = happyGoto action_83
action_154 (26) = happyGoto action_84
action_154 (27) = happyGoto action_10
action_154 (28) = happyGoto action_11
action_154 (29) = happyGoto action_12
action_154 (30) = happyGoto action_13
action_154 _ = happyFail

action_155 (32) = happyShift action_85
action_155 (33) = happyShift action_86
action_155 (34) = happyShift action_87
action_155 (35) = happyShift action_88
action_155 (36) = happyShift action_89
action_155 (37) = happyShift action_90
action_155 (38) = happyShift action_91
action_155 (39) = happyShift action_92
action_155 (42) = happyShift action_93
action_155 (43) = happyShift action_94
action_155 (44) = happyShift action_95
action_155 (45) = happyShift action_96
action_155 (46) = happyShift action_97
action_155 (47) = happyShift action_98
action_155 (48) = happyShift action_99
action_155 (49) = happyShift action_100
action_155 (50) = happyShift action_101
action_155 (51) = happyShift action_102
action_155 (52) = happyShift action_103
action_155 (53) = happyShift action_104
action_155 (54) = happyShift action_105
action_155 (55) = happyShift action_106
action_155 (61) = happyShift action_36
action_155 (65) = happyShift action_37
action_155 (68) = happyShift action_38
action_155 (18) = happyGoto action_181
action_155 (21) = happyGoto action_80
action_155 (22) = happyGoto action_81
action_155 (23) = happyGoto action_82
action_155 (24) = happyGoto action_83
action_155 (26) = happyGoto action_84
action_155 (27) = happyGoto action_10
action_155 (28) = happyGoto action_11
action_155 (29) = happyGoto action_12
action_155 (30) = happyGoto action_13
action_155 _ = happyFail

action_156 (32) = happyShift action_85
action_156 (33) = happyShift action_86
action_156 (34) = happyShift action_87
action_156 (35) = happyShift action_88
action_156 (36) = happyShift action_89
action_156 (37) = happyShift action_90
action_156 (38) = happyShift action_91
action_156 (39) = happyShift action_92
action_156 (42) = happyShift action_93
action_156 (43) = happyShift action_94
action_156 (44) = happyShift action_95
action_156 (45) = happyShift action_96
action_156 (46) = happyShift action_97
action_156 (47) = happyShift action_98
action_156 (48) = happyShift action_99
action_156 (49) = happyShift action_100
action_156 (50) = happyShift action_101
action_156 (51) = happyShift action_102
action_156 (52) = happyShift action_103
action_156 (53) = happyShift action_104
action_156 (54) = happyShift action_105
action_156 (55) = happyShift action_106
action_156 (61) = happyShift action_36
action_156 (65) = happyShift action_37
action_156 (68) = happyShift action_38
action_156 (18) = happyGoto action_180
action_156 (21) = happyGoto action_80
action_156 (22) = happyGoto action_81
action_156 (23) = happyGoto action_82
action_156 (24) = happyGoto action_83
action_156 (26) = happyGoto action_84
action_156 (27) = happyGoto action_10
action_156 (28) = happyGoto action_11
action_156 (29) = happyGoto action_12
action_156 (30) = happyGoto action_13
action_156 _ = happyFail

action_157 (40) = happyShift action_176
action_157 (41) = happyShift action_177
action_157 (42) = happyShift action_93
action_157 (24) = happyGoto action_174
action_157 (25) = happyGoto action_179
action_157 _ = happyFail

action_158 (32) = happyShift action_85
action_158 (33) = happyShift action_86
action_158 (34) = happyShift action_87
action_158 (35) = happyShift action_88
action_158 (36) = happyShift action_89
action_158 (37) = happyShift action_90
action_158 (38) = happyShift action_91
action_158 (39) = happyShift action_92
action_158 (42) = happyShift action_93
action_158 (43) = happyShift action_94
action_158 (44) = happyShift action_95
action_158 (45) = happyShift action_96
action_158 (46) = happyShift action_97
action_158 (47) = happyShift action_98
action_158 (48) = happyShift action_99
action_158 (49) = happyShift action_100
action_158 (50) = happyShift action_101
action_158 (51) = happyShift action_102
action_158 (52) = happyShift action_103
action_158 (53) = happyShift action_104
action_158 (54) = happyShift action_105
action_158 (55) = happyShift action_106
action_158 (61) = happyShift action_36
action_158 (65) = happyShift action_37
action_158 (68) = happyShift action_38
action_158 (18) = happyGoto action_178
action_158 (21) = happyGoto action_80
action_158 (22) = happyGoto action_81
action_158 (23) = happyGoto action_82
action_158 (24) = happyGoto action_83
action_158 (26) = happyGoto action_84
action_158 (27) = happyGoto action_10
action_158 (28) = happyGoto action_11
action_158 (29) = happyGoto action_12
action_158 (30) = happyGoto action_13
action_158 _ = happyFail

action_159 (40) = happyShift action_176
action_159 (41) = happyShift action_177
action_159 (42) = happyShift action_93
action_159 (24) = happyGoto action_174
action_159 (25) = happyGoto action_175
action_159 _ = happyFail

action_160 (32) = happyShift action_85
action_160 (33) = happyShift action_86
action_160 (34) = happyShift action_87
action_160 (35) = happyShift action_88
action_160 (36) = happyShift action_89
action_160 (37) = happyShift action_90
action_160 (38) = happyShift action_91
action_160 (39) = happyShift action_92
action_160 (42) = happyShift action_93
action_160 (43) = happyShift action_94
action_160 (44) = happyShift action_95
action_160 (45) = happyShift action_96
action_160 (46) = happyShift action_97
action_160 (47) = happyShift action_98
action_160 (48) = happyShift action_99
action_160 (49) = happyShift action_100
action_160 (50) = happyShift action_101
action_160 (51) = happyShift action_102
action_160 (52) = happyShift action_103
action_160 (53) = happyShift action_104
action_160 (54) = happyShift action_105
action_160 (55) = happyShift action_106
action_160 (61) = happyShift action_36
action_160 (65) = happyShift action_37
action_160 (68) = happyShift action_38
action_160 (18) = happyGoto action_173
action_160 (21) = happyGoto action_80
action_160 (22) = happyGoto action_81
action_160 (23) = happyGoto action_82
action_160 (24) = happyGoto action_83
action_160 (26) = happyGoto action_84
action_160 (27) = happyGoto action_10
action_160 (28) = happyGoto action_11
action_160 (29) = happyGoto action_12
action_160 (30) = happyGoto action_13
action_160 _ = happyFail

action_161 (38) = happyShift action_91
action_161 (39) = happyShift action_92
action_161 (42) = happyShift action_93
action_161 (20) = happyGoto action_170
action_161 (23) = happyGoto action_171
action_161 (24) = happyGoto action_172
action_161 _ = happyFail

action_162 (36) = happyShift action_89
action_162 (37) = happyShift action_90
action_162 (38) = happyShift action_91
action_162 (39) = happyShift action_92
action_162 (42) = happyShift action_93
action_162 (19) = happyGoto action_166
action_162 (22) = happyGoto action_167
action_162 (23) = happyGoto action_168
action_162 (24) = happyGoto action_169
action_162 _ = happyFail

action_163 (65) = happyShift action_108
action_163 (10) = happyGoto action_165
action_163 _ = happyFail

action_164 _ = happyReduce_8

action_165 (32) = happyShift action_85
action_165 (33) = happyShift action_86
action_165 (34) = happyShift action_87
action_165 (35) = happyShift action_88
action_165 (36) = happyShift action_89
action_165 (37) = happyShift action_90
action_165 (38) = happyShift action_91
action_165 (39) = happyShift action_92
action_165 (42) = happyShift action_93
action_165 (43) = happyShift action_94
action_165 (44) = happyShift action_95
action_165 (45) = happyShift action_96
action_165 (46) = happyShift action_97
action_165 (47) = happyShift action_98
action_165 (48) = happyShift action_99
action_165 (49) = happyShift action_100
action_165 (50) = happyShift action_101
action_165 (51) = happyShift action_102
action_165 (52) = happyShift action_103
action_165 (53) = happyShift action_104
action_165 (54) = happyShift action_105
action_165 (55) = happyShift action_106
action_165 (61) = happyShift action_36
action_165 (65) = happyShift action_37
action_165 (68) = happyShift action_38
action_165 (18) = happyGoto action_219
action_165 (21) = happyGoto action_80
action_165 (22) = happyGoto action_81
action_165 (23) = happyGoto action_82
action_165 (24) = happyGoto action_83
action_165 (26) = happyGoto action_84
action_165 (27) = happyGoto action_10
action_165 (28) = happyGoto action_11
action_165 (29) = happyGoto action_12
action_165 (30) = happyGoto action_13
action_165 _ = happyFail

action_166 (64) = happyShift action_218
action_166 _ = happyFail

action_167 _ = happyReduce_66

action_168 _ = happyReduce_67

action_169 _ = happyReduce_68

action_170 (64) = happyShift action_217
action_170 _ = happyFail

action_171 _ = happyReduce_69

action_172 _ = happyReduce_70

action_173 (58) = happyShift action_216
action_173 _ = happyFail

action_174 _ = happyReduce_80

action_175 (64) = happyShift action_215
action_175 _ = happyFail

action_176 _ = happyReduce_78

action_177 _ = happyReduce_79

action_178 (58) = happyShift action_214
action_178 _ = happyFail

action_179 (64) = happyShift action_213
action_179 _ = happyFail

action_180 (58) = happyShift action_212
action_180 _ = happyFail

action_181 (64) = happyShift action_211
action_181 _ = happyFail

action_182 (64) = happyShift action_210
action_182 _ = happyFail

action_183 (64) = happyShift action_209
action_183 _ = happyFail

action_184 (64) = happyShift action_208
action_184 _ = happyFail

action_185 (58) = happyShift action_207
action_185 _ = happyFail

action_186 (58) = happyShift action_206
action_186 _ = happyFail

action_187 (60) = happyShift action_205
action_187 _ = happyFail

action_188 _ = happyReduce_29

action_189 (58) = happyShift action_204
action_189 _ = happyFail

action_190 (58) = happyShift action_203
action_190 _ = happyFail

action_191 (58) = happyShift action_202
action_191 _ = happyFail

action_192 (58) = happyShift action_201
action_192 _ = happyFail

action_193 (58) = happyShift action_200
action_193 _ = happyFail

action_194 (58) = happyShift action_199
action_194 _ = happyFail

action_195 (58) = happyShift action_198
action_195 _ = happyFail

action_196 (58) = happyShift action_197
action_196 _ = happyFail

action_197 _ = happyReduce_25

action_198 _ = happyReduce_24

action_199 _ = happyReduce_23

action_200 _ = happyReduce_22

action_201 _ = happyReduce_18

action_202 _ = happyReduce_16

action_203 _ = happyReduce_10

action_204 _ = happyReduce_9

action_205 _ = happyReduce_28

action_206 _ = happyReduce_64

action_207 _ = happyReduce_63

action_208 (32) = happyShift action_85
action_208 (33) = happyShift action_86
action_208 (34) = happyShift action_87
action_208 (35) = happyShift action_88
action_208 (36) = happyShift action_89
action_208 (37) = happyShift action_90
action_208 (38) = happyShift action_91
action_208 (39) = happyShift action_92
action_208 (42) = happyShift action_93
action_208 (43) = happyShift action_94
action_208 (44) = happyShift action_95
action_208 (45) = happyShift action_96
action_208 (46) = happyShift action_97
action_208 (47) = happyShift action_98
action_208 (48) = happyShift action_99
action_208 (49) = happyShift action_100
action_208 (50) = happyShift action_101
action_208 (51) = happyShift action_102
action_208 (52) = happyShift action_103
action_208 (53) = happyShift action_104
action_208 (54) = happyShift action_105
action_208 (55) = happyShift action_106
action_208 (61) = happyShift action_36
action_208 (65) = happyShift action_37
action_208 (68) = happyShift action_38
action_208 (18) = happyGoto action_228
action_208 (21) = happyGoto action_80
action_208 (22) = happyGoto action_81
action_208 (23) = happyGoto action_82
action_208 (24) = happyGoto action_83
action_208 (26) = happyGoto action_84
action_208 (27) = happyGoto action_10
action_208 (28) = happyGoto action_11
action_208 (29) = happyGoto action_12
action_208 (30) = happyGoto action_13
action_208 _ = happyFail

action_209 (32) = happyShift action_85
action_209 (33) = happyShift action_86
action_209 (34) = happyShift action_87
action_209 (35) = happyShift action_88
action_209 (36) = happyShift action_89
action_209 (37) = happyShift action_90
action_209 (38) = happyShift action_91
action_209 (39) = happyShift action_92
action_209 (42) = happyShift action_93
action_209 (43) = happyShift action_94
action_209 (44) = happyShift action_95
action_209 (45) = happyShift action_96
action_209 (46) = happyShift action_97
action_209 (47) = happyShift action_98
action_209 (48) = happyShift action_99
action_209 (49) = happyShift action_100
action_209 (50) = happyShift action_101
action_209 (51) = happyShift action_102
action_209 (52) = happyShift action_103
action_209 (53) = happyShift action_104
action_209 (54) = happyShift action_105
action_209 (55) = happyShift action_106
action_209 (61) = happyShift action_36
action_209 (65) = happyShift action_37
action_209 (68) = happyShift action_38
action_209 (18) = happyGoto action_227
action_209 (21) = happyGoto action_80
action_209 (22) = happyGoto action_81
action_209 (23) = happyGoto action_82
action_209 (24) = happyGoto action_83
action_209 (26) = happyGoto action_84
action_209 (27) = happyGoto action_10
action_209 (28) = happyGoto action_11
action_209 (29) = happyGoto action_12
action_209 (30) = happyGoto action_13
action_209 _ = happyFail

action_210 (32) = happyShift action_85
action_210 (33) = happyShift action_86
action_210 (34) = happyShift action_87
action_210 (35) = happyShift action_88
action_210 (36) = happyShift action_89
action_210 (37) = happyShift action_90
action_210 (38) = happyShift action_91
action_210 (39) = happyShift action_92
action_210 (42) = happyShift action_93
action_210 (43) = happyShift action_94
action_210 (44) = happyShift action_95
action_210 (45) = happyShift action_96
action_210 (46) = happyShift action_97
action_210 (47) = happyShift action_98
action_210 (48) = happyShift action_99
action_210 (49) = happyShift action_100
action_210 (50) = happyShift action_101
action_210 (51) = happyShift action_102
action_210 (52) = happyShift action_103
action_210 (53) = happyShift action_104
action_210 (54) = happyShift action_105
action_210 (55) = happyShift action_106
action_210 (61) = happyShift action_36
action_210 (65) = happyShift action_37
action_210 (68) = happyShift action_38
action_210 (18) = happyGoto action_226
action_210 (21) = happyGoto action_80
action_210 (22) = happyGoto action_81
action_210 (23) = happyGoto action_82
action_210 (24) = happyGoto action_83
action_210 (26) = happyGoto action_84
action_210 (27) = happyGoto action_10
action_210 (28) = happyGoto action_11
action_210 (29) = happyGoto action_12
action_210 (30) = happyGoto action_13
action_210 _ = happyFail

action_211 (32) = happyShift action_85
action_211 (33) = happyShift action_86
action_211 (34) = happyShift action_87
action_211 (35) = happyShift action_88
action_211 (36) = happyShift action_89
action_211 (37) = happyShift action_90
action_211 (38) = happyShift action_91
action_211 (39) = happyShift action_92
action_211 (42) = happyShift action_93
action_211 (43) = happyShift action_94
action_211 (44) = happyShift action_95
action_211 (45) = happyShift action_96
action_211 (46) = happyShift action_97
action_211 (47) = happyShift action_98
action_211 (48) = happyShift action_99
action_211 (49) = happyShift action_100
action_211 (50) = happyShift action_101
action_211 (51) = happyShift action_102
action_211 (52) = happyShift action_103
action_211 (53) = happyShift action_104
action_211 (54) = happyShift action_105
action_211 (55) = happyShift action_106
action_211 (61) = happyShift action_36
action_211 (65) = happyShift action_37
action_211 (68) = happyShift action_38
action_211 (18) = happyGoto action_225
action_211 (21) = happyGoto action_80
action_211 (22) = happyGoto action_81
action_211 (23) = happyGoto action_82
action_211 (24) = happyGoto action_83
action_211 (26) = happyGoto action_84
action_211 (27) = happyGoto action_10
action_211 (28) = happyGoto action_11
action_211 (29) = happyGoto action_12
action_211 (30) = happyGoto action_13
action_211 _ = happyFail

action_212 _ = happyReduce_58

action_213 (32) = happyShift action_85
action_213 (33) = happyShift action_86
action_213 (34) = happyShift action_87
action_213 (35) = happyShift action_88
action_213 (36) = happyShift action_89
action_213 (37) = happyShift action_90
action_213 (38) = happyShift action_91
action_213 (39) = happyShift action_92
action_213 (42) = happyShift action_93
action_213 (43) = happyShift action_94
action_213 (44) = happyShift action_95
action_213 (45) = happyShift action_96
action_213 (46) = happyShift action_97
action_213 (47) = happyShift action_98
action_213 (48) = happyShift action_99
action_213 (49) = happyShift action_100
action_213 (50) = happyShift action_101
action_213 (51) = happyShift action_102
action_213 (52) = happyShift action_103
action_213 (53) = happyShift action_104
action_213 (54) = happyShift action_105
action_213 (55) = happyShift action_106
action_213 (61) = happyShift action_36
action_213 (65) = happyShift action_37
action_213 (68) = happyShift action_38
action_213 (18) = happyGoto action_224
action_213 (21) = happyGoto action_80
action_213 (22) = happyGoto action_81
action_213 (23) = happyGoto action_82
action_213 (24) = happyGoto action_83
action_213 (26) = happyGoto action_84
action_213 (27) = happyGoto action_10
action_213 (28) = happyGoto action_11
action_213 (29) = happyGoto action_12
action_213 (30) = happyGoto action_13
action_213 _ = happyFail

action_214 _ = happyReduce_54

action_215 (32) = happyShift action_85
action_215 (33) = happyShift action_86
action_215 (34) = happyShift action_87
action_215 (35) = happyShift action_88
action_215 (36) = happyShift action_89
action_215 (37) = happyShift action_90
action_215 (38) = happyShift action_91
action_215 (39) = happyShift action_92
action_215 (42) = happyShift action_93
action_215 (43) = happyShift action_94
action_215 (44) = happyShift action_95
action_215 (45) = happyShift action_96
action_215 (46) = happyShift action_97
action_215 (47) = happyShift action_98
action_215 (48) = happyShift action_99
action_215 (49) = happyShift action_100
action_215 (50) = happyShift action_101
action_215 (51) = happyShift action_102
action_215 (52) = happyShift action_103
action_215 (53) = happyShift action_104
action_215 (54) = happyShift action_105
action_215 (55) = happyShift action_106
action_215 (61) = happyShift action_36
action_215 (65) = happyShift action_37
action_215 (68) = happyShift action_38
action_215 (18) = happyGoto action_223
action_215 (21) = happyGoto action_80
action_215 (22) = happyGoto action_81
action_215 (23) = happyGoto action_82
action_215 (24) = happyGoto action_83
action_215 (26) = happyGoto action_84
action_215 (27) = happyGoto action_10
action_215 (28) = happyGoto action_11
action_215 (29) = happyGoto action_12
action_215 (30) = happyGoto action_13
action_215 _ = happyFail

action_216 _ = happyReduce_52

action_217 (32) = happyShift action_85
action_217 (33) = happyShift action_86
action_217 (34) = happyShift action_87
action_217 (35) = happyShift action_88
action_217 (36) = happyShift action_89
action_217 (37) = happyShift action_90
action_217 (38) = happyShift action_91
action_217 (39) = happyShift action_92
action_217 (42) = happyShift action_93
action_217 (43) = happyShift action_94
action_217 (44) = happyShift action_95
action_217 (45) = happyShift action_96
action_217 (46) = happyShift action_97
action_217 (47) = happyShift action_98
action_217 (48) = happyShift action_99
action_217 (49) = happyShift action_100
action_217 (50) = happyShift action_101
action_217 (51) = happyShift action_102
action_217 (52) = happyShift action_103
action_217 (53) = happyShift action_104
action_217 (54) = happyShift action_105
action_217 (55) = happyShift action_106
action_217 (61) = happyShift action_36
action_217 (65) = happyShift action_37
action_217 (68) = happyShift action_38
action_217 (18) = happyGoto action_222
action_217 (21) = happyGoto action_80
action_217 (22) = happyGoto action_81
action_217 (23) = happyGoto action_82
action_217 (24) = happyGoto action_83
action_217 (26) = happyGoto action_84
action_217 (27) = happyGoto action_10
action_217 (28) = happyGoto action_11
action_217 (29) = happyGoto action_12
action_217 (30) = happyGoto action_13
action_217 _ = happyFail

action_218 (32) = happyShift action_85
action_218 (33) = happyShift action_86
action_218 (34) = happyShift action_87
action_218 (35) = happyShift action_88
action_218 (36) = happyShift action_89
action_218 (37) = happyShift action_90
action_218 (38) = happyShift action_91
action_218 (39) = happyShift action_92
action_218 (42) = happyShift action_93
action_218 (43) = happyShift action_94
action_218 (44) = happyShift action_95
action_218 (45) = happyShift action_96
action_218 (46) = happyShift action_97
action_218 (47) = happyShift action_98
action_218 (48) = happyShift action_99
action_218 (49) = happyShift action_100
action_218 (50) = happyShift action_101
action_218 (51) = happyShift action_102
action_218 (52) = happyShift action_103
action_218 (53) = happyShift action_104
action_218 (54) = happyShift action_105
action_218 (55) = happyShift action_106
action_218 (61) = happyShift action_36
action_218 (65) = happyShift action_37
action_218 (68) = happyShift action_38
action_218 (18) = happyGoto action_221
action_218 (21) = happyGoto action_80
action_218 (22) = happyGoto action_81
action_218 (23) = happyGoto action_82
action_218 (24) = happyGoto action_83
action_218 (26) = happyGoto action_84
action_218 (27) = happyGoto action_10
action_218 (28) = happyGoto action_11
action_218 (29) = happyGoto action_12
action_218 (30) = happyGoto action_13
action_218 _ = happyFail

action_219 (65) = happyShift action_108
action_219 (10) = happyGoto action_220
action_219 _ = happyFail

action_220 (60) = happyShift action_237
action_220 _ = happyFail

action_221 (58) = happyShift action_236
action_221 _ = happyFail

action_222 (58) = happyShift action_235
action_222 _ = happyFail

action_223 (58) = happyShift action_234
action_223 _ = happyFail

action_224 (58) = happyShift action_233
action_224 _ = happyFail

action_225 (58) = happyShift action_232
action_225 _ = happyFail

action_226 (58) = happyShift action_231
action_226 _ = happyFail

action_227 (58) = happyShift action_230
action_227 _ = happyFail

action_228 (58) = happyShift action_229
action_228 _ = happyFail

action_229 _ = happyReduce_62

action_230 _ = happyReduce_61

action_231 _ = happyReduce_60

action_232 _ = happyReduce_59

action_233 _ = happyReduce_55

action_234 _ = happyReduce_53

action_235 _ = happyReduce_47

action_236 _ = happyReduce_46

action_237 _ = happyReduce_65

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (([], happy_var_1)
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ((happy_var_1,happy_var_2)
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1:happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyTerminal ((TokenFileName happy_var_2, _)))
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1:happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 8 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (makeRewriteRule happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 6 9 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (binaryMatcher (binaryNodeMatcher NodeAt) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 6 9 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (binaryMatcher (binaryNodeMatcher NodeDown) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 9 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((TokenNodeVal NodeDia happy_var_1, _))) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (unaryMatcher (binaryNodeMatcher NodeDia (leafMatcher $ LV NodeRel happy_var_1)) happy_var_3
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 9 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (binaryMatcher (binaryNodeMatcher NodeDia)  happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 4 9 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((TokenNodeVal NodeBox happy_var_1, _))) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (unaryMatcher (binaryNodeMatcher NodeBox (leafMatcher $ LV NodeRel happy_var_1)) happy_var_3
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 6 9 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (binaryMatcher (binaryNodeMatcher NodeBox) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  9 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn9
		 (atomicMatcher (leafMatcher (L NodeTrue))
	)

happyReduce_20 = happySpecReduce_1  9 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn9
		 (atomicMatcher (leafMatcher (L NodeFalse))
	)

happyReduce_21 = happyReduce 4 9 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (unaryMatcher (unaryNodeMatcher NodeNeg) happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 6 9 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (binaryMatcher (binaryNodeMatcher NodeAnd) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 6 9 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (binaryMatcher (binaryNodeMatcher NodeOr) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 6 9 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (binaryMatcher (binaryNodeMatcher NodeDimp) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 6 9 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (binaryMatcher (binaryNodeMatcher NodeImp) happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 4 9 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (unaryMatcher (unaryNodeMatcher NodeUBox) happy_var_3
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 4 9 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (unaryMatcher (unaryNodeMatcher NodeUDia) happy_var_3
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 6 9 happyReduction_28
happyReduction_28 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (contextMatcher (simpleRegExpMatcher happy_var_1) happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_3  10 happyReduction_29
happyReduction_29 _
	_
	_
	 =  HappyAbsSyn10
		 (()
	)

happyReduce_30 = happySpecReduce_1  11 happyReduction_30
happyReduction_30 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  11 happyReduction_31
happyReduction_31 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  11 happyReduction_32
happyReduction_32 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  12 happyReduction_33
happyReduction_33 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  12 happyReduction_34
happyReduction_34 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  12 happyReduction_35
happyReduction_35 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  13 happyReduction_36
happyReduction_36 (HappyTerminal ((TokenNodeVal NodeProp happy_var_1, _)))
	 =  HappyAbsSyn9
		 (atomicMatcher (leafMatcher (LV NodeProp happy_var_1))
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  13 happyReduction_37
happyReduction_37 (HappyTerminal ((TokenLeafVar NodeProp happy_var_1, _)))
	 =  HappyAbsSyn9
		 (matcherWithTermVar (leafVarMatcher NodeProp happy_var_1) happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  14 happyReduction_38
happyReduction_38 (HappyTerminal ((TokenNodeVal NodeNom happy_var_1, _)))
	 =  HappyAbsSyn9
		 (atomicMatcher (leafMatcher (LV NodeNom happy_var_1))
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  14 happyReduction_39
happyReduction_39 (HappyTerminal ((TokenLeafVar NodeNom happy_var_1, _)))
	 =  HappyAbsSyn9
		 (matcherWithTermVar (leafVarMatcher NodeNom happy_var_1) happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  15 happyReduction_40
happyReduction_40 (HappyTerminal ((TokenNodeVal NodeVar happy_var_1 , _)))
	 =  HappyAbsSyn9
		 (atomicMatcher (leafMatcher (LV NodeVar happy_var_1))
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  15 happyReduction_41
happyReduction_41 (HappyTerminal ((TokenLeafVar NodeVar happy_var_1, _)))
	 =  HappyAbsSyn9
		 (matcherWithTermVar (leafVarMatcher NodeVar happy_var_1) happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  16 happyReduction_42
happyReduction_42 (HappyTerminal ((TokenVar happy_var_1, _)))
	 =  HappyAbsSyn9
		 (matcherWithTermVar (varMatcher happy_var_1) happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  17 happyReduction_43
happyReduction_43 (HappyTerminal ((TokenNodeVal NodeRel happy_var_1, _)))
	 =  HappyAbsSyn9
		 (atomicMatcher (leafMatcher (LV NodeRel happy_var_1))
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  17 happyReduction_44
happyReduction_44 (HappyTerminal ((TokenLeafVar NodeRel happy_var_1, _)))
	 =  HappyAbsSyn9
		 (matcherWithTermVar (leafVarMatcher NodeRel happy_var_1) happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  17 happyReduction_45
happyReduction_45 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happyReduce 6 18 happyReduction_46
happyReduction_46 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((writeBinaryNode NodeAt happy_var_3 (fst happy_var_5), snd happy_var_5)
	) `HappyStk` happyRest

happyReduce_47 = happyReduce 6 18 happyReduction_47
happyReduction_47 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((writeBinaryNode NodeDown happy_var_3 (fst happy_var_5), snd happy_var_5)
	) `HappyStk` happyRest

happyReduce_48 = happySpecReduce_1  18 happyReduction_48
happyReduction_48 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 ((happy_var_1, [])
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  18 happyReduction_49
happyReduction_49 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 ((happy_var_1, [])
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  18 happyReduction_50
happyReduction_50 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 ((happy_var_1, [])
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  18 happyReduction_51
happyReduction_51 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 ((happy_var_1, [])
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happyReduce 4 18 happyReduction_52
happyReduction_52 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((TokenNodeVal NodeDia happy_var_1, _))) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((writeBinaryNode NodeDia (writeLeaf NodeRel $ Just happy_var_1) (fst happy_var_3), snd happy_var_3)
	) `HappyStk` happyRest

happyReduce_53 = happyReduce 6 18 happyReduction_53
happyReduction_53 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((writeBinaryNode NodeDia happy_var_3 (fst happy_var_5), snd happy_var_5)
	) `HappyStk` happyRest

happyReduce_54 = happyReduce 4 18 happyReduction_54
happyReduction_54 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((TokenNodeVal NodeBox happy_var_1, _))) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((writeBinaryNode NodeBox (writeLeaf NodeRel $ Just happy_var_1) (fst happy_var_3), snd happy_var_3)
	) `HappyStk` happyRest

happyReduce_55 = happyReduce 6 18 happyReduction_55
happyReduction_55 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((writeBinaryNode NodeBox happy_var_3 (fst happy_var_5), snd happy_var_5)
	) `HappyStk` happyRest

happyReduce_56 = happySpecReduce_1  18 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn18
		 ((writeLeaf NodeTrue Nothing, [])
	)

happyReduce_57 = happySpecReduce_1  18 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn18
		 ((writeLeaf NodeFalse Nothing, [])
	)

happyReduce_58 = happyReduce 4 18 happyReduction_58
happyReduction_58 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((writeUnaryNode NodeNeg (fst happy_var_3), snd happy_var_3)
	) `HappyStk` happyRest

happyReduce_59 = happyReduce 6 18 happyReduction_59
happyReduction_59 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((writeBinaryNode NodeAnd (fst happy_var_3) (fst happy_var_5), (snd happy_var_3) ++ (snd happy_var_5))
	) `HappyStk` happyRest

happyReduce_60 = happyReduce 6 18 happyReduction_60
happyReduction_60 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((writeBinaryNode NodeOr (fst happy_var_3) (fst happy_var_5), (snd happy_var_3) ++ (snd happy_var_5))
	) `HappyStk` happyRest

happyReduce_61 = happyReduce 6 18 happyReduction_61
happyReduction_61 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((writeBinaryNode NodeDimp (fst happy_var_3) (fst happy_var_5), (snd happy_var_3) ++ (snd happy_var_5))
	) `HappyStk` happyRest

happyReduce_62 = happyReduce 6 18 happyReduction_62
happyReduction_62 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((writeBinaryNode NodeImp (fst happy_var_3) (fst happy_var_5), (snd happy_var_3) ++ (snd happy_var_5))
	) `HappyStk` happyRest

happyReduce_63 = happyReduce 4 18 happyReduction_63
happyReduction_63 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((writeUnaryNode NodeUBox (fst happy_var_3), snd happy_var_3)
	) `HappyStk` happyRest

happyReduce_64 = happyReduce 4 18 happyReduction_64
happyReduction_64 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((writeUnaryNode NodeUDia (fst happy_var_3), snd happy_var_3)
	) `HappyStk` happyRest

happyReduce_65 = happyReduce 6 18 happyReduction_65
happyReduction_65 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 ((writeInContext happy_var_1 (fst happy_var_4), snd happy_var_4)
	) `HappyStk` happyRest

happyReduce_66 = happySpecReduce_1  19 happyReduction_66
happyReduction_66 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  19 happyReduction_67
happyReduction_67 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  19 happyReduction_68
happyReduction_68 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  20 happyReduction_69
happyReduction_69 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  20 happyReduction_70
happyReduction_70 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  21 happyReduction_71
happyReduction_71 (HappyTerminal ((TokenNodeVal NodeProp happy_var_1, _)))
	 =  HappyAbsSyn19
		 (writeLeaf NodeProp $ Just happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  21 happyReduction_72
happyReduction_72 (HappyTerminal ((TokenLeafVar NodeProp happy_var_1, _)))
	 =  HappyAbsSyn19
		 (writeLeafVar NodeProp happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  22 happyReduction_73
happyReduction_73 (HappyTerminal ((TokenNodeVal NodeNom happy_var_1, _)))
	 =  HappyAbsSyn19
		 (writeLeaf NodeNom $ Just happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  22 happyReduction_74
happyReduction_74 (HappyTerminal ((TokenLeafVar NodeNom happy_var_1, _)))
	 =  HappyAbsSyn19
		 (writeLeafVar NodeNom happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  23 happyReduction_75
happyReduction_75 (HappyTerminal ((TokenNodeVal NodeVar happy_var_1 , _)))
	 =  HappyAbsSyn19
		 (writeLeaf NodeVar $ Just happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  23 happyReduction_76
happyReduction_76 (HappyTerminal ((TokenLeafVar NodeVar happy_var_1, _)))
	 =  HappyAbsSyn19
		 (writeLeafVar NodeVar happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_1  24 happyReduction_77
happyReduction_77 (HappyTerminal ((TokenVar happy_var_1, _)))
	 =  HappyAbsSyn19
		 (writeVar happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_1  25 happyReduction_78
happyReduction_78 (HappyTerminal ((TokenNodeVal NodeRel happy_var_1, _)))
	 =  HappyAbsSyn19
		 (writeLeaf NodeRel $ Just happy_var_1
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  25 happyReduction_79
happyReduction_79 (HappyTerminal ((TokenLeafVar NodeRel happy_var_1, _)))
	 =  HappyAbsSyn19
		 (writeLeafVar NodeRel happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  25 happyReduction_80
happyReduction_80 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  26 happyReduction_81
happyReduction_81 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn26
		 ((happy_var_1, -1)
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_2  26 happyReduction_82
happyReduction_82 (HappyTerminal ((TokenSubIndex happy_var_2, _)))
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn26
		 ((happy_var_1, happy_var_2)
	)
happyReduction_82 _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  27 happyReduction_83
happyReduction_83 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_2  27 happyReduction_84
happyReduction_84 (HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1 ++ happy_var_2
	)
happyReduction_84 _ _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  28 happyReduction_85
happyReduction_85 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn27
		 ([Exactly happy_var_1]
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_2  28 happyReduction_86
happyReduction_86 _
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn27
		 ([Exactly happy_var_1, Star happy_var_1]
	)
happyReduction_86 _ _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_2  28 happyReduction_87
happyReduction_87 _
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn27
		 ([Star happy_var_1]
	)
happyReduction_87 _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  29 happyReduction_88
happyReduction_88 _
	 =  HappyAbsSyn29
		 (Anything
	)

happyReduce_89 = happySpecReduce_1  29 happyReduction_89
happyReduction_89 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn29
		 (OneOf [happy_var_1]
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_3  29 happyReduction_90
happyReduction_90 _
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn29
		 (OneOf happy_var_2
	)
happyReduction_90 _ _ _  = notHappyAtAll 

happyReduce_91 = happyReduce 4 29 happyReduction_91
happyReduction_91 (_ `HappyStk`
	(HappyAbsSyn31  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 (NoneOf happy_var_3
	) `HappyStk` happyRest

happyReduce_92 = happySpecReduce_1  30 happyReduction_92
happyReduction_92 _
	 =  HappyAbsSyn30
		 (NodeDia
	)

happyReduce_93 = happySpecReduce_1  30 happyReduction_93
happyReduction_93 _
	 =  HappyAbsSyn30
		 (NodeBox
	)

happyReduce_94 = happySpecReduce_1  30 happyReduction_94
happyReduction_94 _
	 =  HappyAbsSyn30
		 (NodeUDia
	)

happyReduce_95 = happySpecReduce_1  30 happyReduction_95
happyReduction_95 _
	 =  HappyAbsSyn30
		 (NodeUBox
	)

happyReduce_96 = happySpecReduce_1  30 happyReduction_96
happyReduction_96 _
	 =  HappyAbsSyn30
		 (NodeDown
	)

happyReduce_97 = happySpecReduce_1  30 happyReduction_97
happyReduction_97 _
	 =  HappyAbsSyn30
		 (NodeAnd
	)

happyReduce_98 = happySpecReduce_1  30 happyReduction_98
happyReduction_98 _
	 =  HappyAbsSyn30
		 (NodeOr
	)

happyReduce_99 = happySpecReduce_1  30 happyReduction_99
happyReduction_99 _
	 =  HappyAbsSyn30
		 (NodeDimp
	)

happyReduce_100 = happySpecReduce_1  30 happyReduction_100
happyReduction_100 _
	 =  HappyAbsSyn30
		 (NodeImp
	)

happyReduce_101 = happySpecReduce_1  30 happyReduction_101
happyReduction_101 _
	 =  HappyAbsSyn30
		 (NodeNeg
	)

happyReduce_102 = happySpecReduce_1  31 happyReduction_102
happyReduction_102 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn31
		 ([happy_var_1]
	)
happyReduction_102 _  = notHappyAtAll 

happyReduce_103 = happySpecReduce_2  31 happyReduction_103
happyReduction_103 (HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1:happy_var_2
	)
happyReduction_103 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 72 72 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	(TokenNode NodeAt , _) -> cont 32;
	(TokenNode NodeDown , _) -> cont 33;
	(TokenNodeVal NodeProp happy_dollar_dollar, _) -> cont 34;
	(TokenLeafVar NodeProp happy_dollar_dollar, _) -> cont 35;
	(TokenNodeVal NodeNom happy_dollar_dollar, _) -> cont 36;
	(TokenLeafVar NodeNom happy_dollar_dollar, _) -> cont 37;
	(TokenNodeVal NodeVar happy_dollar_dollar , _) -> cont 38;
	(TokenLeafVar NodeVar happy_dollar_dollar, _) -> cont 39;
	(TokenNodeVal NodeRel happy_dollar_dollar, _) -> cont 40;
	(TokenLeafVar NodeRel happy_dollar_dollar, _) -> cont 41;
	(TokenVar happy_dollar_dollar, _) -> cont 42;
	(TokenNodeVal NodeDia happy_dollar_dollar, _) -> cont 43;
	(TokenNode NodeDia, _) -> cont 44;
	(TokenNodeVal NodeBox happy_dollar_dollar, _) -> cont 45;
	(TokenNode NodeBox, _) -> cont 46;
	(TokenNode NodeTrue, _) -> cont 47;
	(TokenNode NodeFalse, _) -> cont 48;
	(TokenNode NodeNeg, _) -> cont 49;
	(TokenNode NodeAnd, _) -> cont 50;
	(TokenNode NodeOr, _) -> cont 51;
	(TokenNode NodeDimp, _) -> cont 52;
	(TokenNode NodeImp, _) -> cont 53;
	(TokenNode NodeUBox, _) -> cont 54;
	(TokenNode NodeUDia, _) -> cont 55;
	(TokenSubIndex happy_dollar_dollar, _) -> cont 56;
	(TokenOB, _) -> cont 57;
	(TokenCB, _) -> cont 58;
	(TokenOSB, _) -> cont 59;
	(TokenCSB, _) -> cont 60;
	(TokenOCB, _) -> cont 61;
	(TokenCCB, _) -> cont 62;
	(TokenSC, _) -> cont 63;
	(TokenC, _) -> cont 64;
	(TokenDot, _) -> cont 65;
	(TokenStar, _) -> cont 66;
	(TokenPlus, _) -> cont 67;
	(TokenTilde, _) -> cont 68;
	(TokenRewrite, _) -> cont 69;
	(TokenInclude, _) -> cont 70;
	(TokenFileName happy_dollar_dollar, _) -> cont 71;
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
happyError' :: () => [((RewriteRulesToken, FilePos))] -> HappyIdentity a
happyError' = HappyIdentity . happyError

rewriteRulesParser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [(RewriteRulesToken, FilePos)] -> a
happyError ((_, fp):_) = error ("Parse error near line " ++ 
                                   (show $ line fp) ++ 
                                   ", col. " ++
                                   (show $ col fp))

data LeftPatternParseInfo = LPI{matcher :: forall m. MonadPlus m => TermMatcherM m HyLoNodeType,
                                contexts :: [Context HyLoNodeType],
                                termVars :: [TermVar]}

unaryMatcher :: (forall m. MonadPlus m => TermMatcherM m HyLoNodeType -> TermMatcherM m HyLoNodeType) -> LeftPatternParseInfo -> LeftPatternParseInfo
unaryMatcher um p  = LPI{matcher = um (matcher p),
                         contexts = contexts p,
                         termVars = termVars p}

atomicMatcher :: (forall m. MonadPlus m => TermMatcherM m HyLoNodeType) -> LeftPatternParseInfo
atomicMatcher m = LPI{matcher = m, contexts = [], termVars = []}

matcherWithTermVar :: (forall m. MonadPlus m => TermMatcherM m HyLoNodeType) -> TermVar -> LeftPatternParseInfo
matcherWithTermVar m v = LPI{matcher = m, contexts = [], termVars = [v]}

binaryMatcher :: (forall m. MonadPlus m => TermMatcherM m HyLoNodeType -> TermMatcherM m HyLoNodeType -> TermMatcherM m HyLoNodeType) -> 
                               LeftPatternParseInfo -> LeftPatternParseInfo -> LeftPatternParseInfo
binaryMatcher bm p1 p2 = LPI{matcher = bm (matcher p1) (matcher p2),
                             contexts = (contexts p1) ++ (contexts p2),
                             termVars = (termVars p1) ++ (termVars p2)}

contextMatcher :: (forall m. MonadPlus m => TermMatcherM m HyLoNodeType -> TermMatcherM m HyLoNodeType) -> 
                                 Context HyLoNodeType -> LeftPatternParseInfo -> LeftPatternParseInfo
contextMatcher um c p = LPI{matcher = um (matcher p),
                            contexts = c:(contexts p),
                            termVars = termVars p}


makeRewriteRule ::  LeftPatternParseInfo -> (TermWriter HyLoNodeType, [Context HyLoNodeType]) -> ASTRewriteRule HyLoNodeType
makeRewriteRule lpi (writer, rightPatterns)
    |containsRepetitions leftPatterns                  = error "Repeated context patterns on left hand of rule are not allowed"
    | not . null $ (nub rightPatterns) \\ leftPatterns = error "Context patterns occurring in right hand side of a rule must occur in left hand too"
    | termVarMatchedTwice                              = rewrite (firstMatch $ matcher lpi) writer  -- a term variable is matched twice
    | otherwise                                        = rewrite (matcher lpi) writer
        where leftPatterns        = contexts lpi
              leftTermVars        = termVars lpi
              termVarMatchedTwice = length leftTermVars /= (length . nub $ leftTermVars)


containsRepetitions :: Eq a => [a] -> Bool
containsRepetitions l = (length . nub $ l) /= length l


showPattern :: Show a => ContextRegExp a -> String
showPattern  []              = []
showPattern ((Exactly n):ps) = (show n) ++ showPattern ps
showPattern ((Star n):ps)    = (show n) ++ "*" ++ showPattern ps

rewriteRulesFromString :: String -> [ASTRewriteRule HyLoNodeType]
rewriteRulesFromString = discardFileInclusions . rewriteRulesParser . rewriteRulesLexer

discardFileInclusions :: ([FilePath], [ASTRewriteRule HyLoNodeType]) -> [ASTRewriteRule HyLoNodeType]
discardFileInclusions ([], rules) = rules
discardFileInclusions _           = error "Can't proccess .include directive when building rules from a string"

rewriteRulesFromFile :: FilePath -> IO [ASTRewriteRule HyLoNodeType]
rewriteRulesFromFile f = loadRules [f] []

loadRules :: [FilePath] -> [(FilePath, [ASTRewriteRule HyLoNodeType])] -> IO [ASTRewriteRule HyLoNodeType]
loadRules [] l     = return $ concatMap snd l
loadRules (f:fs) l =
    do
        if (isNothing $ lookup f l)
            then do
                    let lexAndParse = rewriteRulesParser . rewriteRulesLexer
                    fileContent <- readFile f
                    let (includes, rules) = lexAndParse fileContent
                    loadRules (includes ++ fs) $ (f,rules):l
            else loadRules fs l
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
