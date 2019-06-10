{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParGrammar where
import AbsGrammar
import LexGrammar
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (String)
	| HappyAbsSyn5 (Integer)
	| HappyAbsSyn6 (LowerIdent)
	| HappyAbsSyn7 (UpperIdent)
	| HappyAbsSyn8 ([LowerIdent])
	| HappyAbsSyn9 ([UpperIdent])
	| HappyAbsSyn10 (Program)
	| HappyAbsSyn11 (Import)
	| HappyAbsSyn12 ([Import])
	| HappyAbsSyn13 (Type)
	| HappyAbsSyn14 ([Type])
	| HappyAbsSyn17 (Pat)
	| HappyAbsSyn19 ([Pat])
	| HappyAbsSyn20 (Branch)
	| HappyAbsSyn21 ([Branch])
	| HappyAbsSyn22 (Exp)
	| HappyAbsSyn28 ([Exp])
	| HappyAbsSyn31 (DataDecl)
	| HappyAbsSyn32 ([DataDecl])
	| HappyAbsSyn33 (Decl)
	| HappyAbsSyn34 ([Decl])
	| HappyAbsSyn36 (Constr)
	| HappyAbsSyn37 ([Constr])
	| HappyAbsSyn38 (Lit)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
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
 action_121 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

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
 happyReduce_69 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,393) ([0,0,0,256,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,8192,0,0,0,0,32,0,0,0,0,0,0,0,16,0,0,0,4,16384,0,0,0,0,16,0,0,0,256,0,0,0,0,0,0,16384,0,4,0,0,4,0,0,0,0,0,0,0,32,0,2,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,1024,0,0,24584,49298,1,0,0,0,16,0,0,32,0,0,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,14336,0,0,128,0,0,0,40960,0,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,4684,56,0,8192,18816,1794,0,0,0,16384,0,0,128,2342,28,0,4096,9408,897,0,0,0,256,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,32800,585,7,0,0,0,64,0,0,0,32,0,0,0,512,0,0,32768,0,0,0,0,4,0,0,0,32768,0,0,0,512,0,0,0,8192,0,1792,0,0,4,57344,0,0,128,0,28,0,4096,0,896,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,4,0,1024,0,192,0,0,0,0,0,0,64,0,0,0,2048,0,0,0,0,10,0,0,0,0,0,0,0,0,0,0,0,8192,18816,1794,0,0,12292,57417,0,0,0,16384,0,0,4096,9408,897,0,0,512,0,0,0,0,0,0,0,2048,37472,448,0,0,0,1,0,0,32,4,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,8192,0,1536,0,0,0,0,0,0,256,0,0,0,4096,0,768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,2,0,0,0,0,256,0,0,0,0,0,0,512,64,112,0,0,0,0,0,0,24584,49298,1,0,0,0,2,0,0,0,1,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,16386,28672,0,0,64,1171,14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,37632,3588,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","String","Integer","LowerIdent","UpperIdent","ListLowerIdent","ListUpperIdent","Program","Import","ListImport","Type1","ListType","Type","Type2","Pat1","Pat","ListPat","Branch","ListBranch","Exp7","Exp6","Exp4","Exp3","Exp2","Exp","ListExp","Exp1","Exp5","DataDecl","ListDataDecl","Decl","ListDecl","Decl1","Constr","ListConstr","Lit","'('","')'","'*'","'+'","','","'-'","'->'","';'","'='","'=='","'['","'\\\\'","']'","'_'","'case'","'data'","'else'","'if'","'import'","'in'","'let'","'of'","'then'","'{'","'|'","'}'","L_quoted","L_integ","L_LowerIdent","L_UpperIdent","%eof"]
        bit_start = st * 69
        bit_end = (st + 1) * 69
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..68]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (57) = happyShift action_6
action_0 (10) = happyGoto action_3
action_0 (11) = happyGoto action_4
action_0 (12) = happyGoto action_5
action_0 _ = happyReduce_11

action_1 (65) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (69) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (46) = happyShift action_9
action_4 _ = happyReduce_12

action_5 (32) = happyGoto action_8
action_5 _ = happyReduce_58

action_6 (65) = happyShift action_2
action_6 (4) = happyGoto action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_10

action_8 (54) = happyShift action_13
action_8 (31) = happyGoto action_11
action_8 (34) = happyGoto action_12
action_8 _ = happyReduce_63

action_9 (57) = happyShift action_6
action_9 (11) = happyGoto action_4
action_9 (12) = happyGoto action_10
action_9 _ = happyReduce_11

action_10 _ = happyReduce_13

action_11 (46) = happyShift action_21
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (39) = happyShift action_19
action_12 (67) = happyShift action_20
action_12 (6) = happyGoto action_16
action_12 (33) = happyGoto action_17
action_12 (35) = happyGoto action_18
action_12 _ = happyReduce_9

action_13 (68) = happyShift action_15
action_13 (7) = happyGoto action_14
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (67) = happyShift action_20
action_14 (6) = happyGoto action_26
action_14 (8) = happyGoto action_27
action_14 _ = happyReduce_5

action_15 _ = happyReduce_4

action_16 (47) = happyShift action_25
action_16 (67) = happyShift action_20
action_16 (6) = happyGoto action_24
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (46) = happyShift action_23
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_62

action_19 (39) = happyShift action_19
action_19 (67) = happyShift action_20
action_19 (6) = happyGoto action_16
action_19 (33) = happyGoto action_22
action_19 (35) = happyGoto action_18
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_3

action_21 _ = happyReduce_59

action_22 (40) = happyShift action_50
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_64

action_24 (67) = happyShift action_20
action_24 (6) = happyGoto action_26
action_24 (8) = happyGoto action_49
action_24 _ = happyReduce_5

action_25 (39) = happyShift action_42
action_25 (49) = happyShift action_43
action_25 (50) = happyShift action_44
action_25 (53) = happyShift action_45
action_25 (56) = happyShift action_46
action_25 (59) = happyShift action_47
action_25 (66) = happyShift action_48
action_25 (67) = happyShift action_20
action_25 (68) = happyShift action_15
action_25 (5) = happyGoto action_30
action_25 (6) = happyGoto action_31
action_25 (7) = happyGoto action_32
action_25 (22) = happyGoto action_33
action_25 (23) = happyGoto action_34
action_25 (24) = happyGoto action_35
action_25 (25) = happyGoto action_36
action_25 (26) = happyGoto action_37
action_25 (27) = happyGoto action_38
action_25 (29) = happyGoto action_39
action_25 (30) = happyGoto action_40
action_25 (38) = happyGoto action_41
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (67) = happyShift action_20
action_26 (6) = happyGoto action_26
action_26 (8) = happyGoto action_29
action_26 _ = happyReduce_5

action_27 (47) = happyShift action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (68) = happyShift action_15
action_28 (7) = happyGoto action_64
action_28 (36) = happyGoto action_65
action_28 (37) = happyGoto action_66
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_6

action_30 _ = happyReduce_69

action_31 _ = happyReduce_33

action_32 _ = happyReduce_34

action_33 _ = happyReduce_38

action_34 (39) = happyShift action_42
action_34 (66) = happyShift action_48
action_34 (67) = happyShift action_20
action_34 (68) = happyShift action_15
action_34 (5) = happyGoto action_30
action_34 (6) = happyGoto action_31
action_34 (7) = happyGoto action_32
action_34 (22) = happyGoto action_63
action_34 (38) = happyGoto action_41
action_34 _ = happyReduce_56

action_35 (41) = happyShift action_62
action_35 _ = happyReduce_43

action_36 (42) = happyShift action_60
action_36 (44) = happyShift action_61
action_36 _ = happyReduce_45

action_37 (48) = happyShift action_59
action_37 _ = happyReduce_55

action_38 _ = happyReduce_60

action_39 _ = happyReduce_51

action_40 _ = happyReduce_40

action_41 _ = happyReduce_35

action_42 (39) = happyShift action_42
action_42 (49) = happyShift action_43
action_42 (50) = happyShift action_44
action_42 (53) = happyShift action_45
action_42 (56) = happyShift action_46
action_42 (59) = happyShift action_47
action_42 (66) = happyShift action_48
action_42 (67) = happyShift action_20
action_42 (68) = happyShift action_15
action_42 (5) = happyGoto action_30
action_42 (6) = happyGoto action_31
action_42 (7) = happyGoto action_32
action_42 (22) = happyGoto action_33
action_42 (23) = happyGoto action_34
action_42 (24) = happyGoto action_35
action_42 (25) = happyGoto action_36
action_42 (26) = happyGoto action_37
action_42 (27) = happyGoto action_58
action_42 (29) = happyGoto action_39
action_42 (30) = happyGoto action_40
action_42 (38) = happyGoto action_41
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (39) = happyShift action_42
action_43 (49) = happyShift action_43
action_43 (50) = happyShift action_44
action_43 (53) = happyShift action_45
action_43 (56) = happyShift action_46
action_43 (59) = happyShift action_47
action_43 (66) = happyShift action_48
action_43 (67) = happyShift action_20
action_43 (68) = happyShift action_15
action_43 (5) = happyGoto action_30
action_43 (6) = happyGoto action_31
action_43 (7) = happyGoto action_32
action_43 (22) = happyGoto action_33
action_43 (23) = happyGoto action_34
action_43 (24) = happyGoto action_35
action_43 (25) = happyGoto action_36
action_43 (26) = happyGoto action_37
action_43 (27) = happyGoto action_56
action_43 (28) = happyGoto action_57
action_43 (29) = happyGoto action_39
action_43 (30) = happyGoto action_40
action_43 (38) = happyGoto action_41
action_43 _ = happyReduce_52

action_44 (67) = happyShift action_20
action_44 (6) = happyGoto action_55
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (39) = happyShift action_42
action_45 (49) = happyShift action_43
action_45 (50) = happyShift action_44
action_45 (53) = happyShift action_45
action_45 (56) = happyShift action_46
action_45 (59) = happyShift action_47
action_45 (66) = happyShift action_48
action_45 (67) = happyShift action_20
action_45 (68) = happyShift action_15
action_45 (5) = happyGoto action_30
action_45 (6) = happyGoto action_31
action_45 (7) = happyGoto action_32
action_45 (22) = happyGoto action_33
action_45 (23) = happyGoto action_34
action_45 (24) = happyGoto action_35
action_45 (25) = happyGoto action_36
action_45 (26) = happyGoto action_37
action_45 (27) = happyGoto action_54
action_45 (29) = happyGoto action_39
action_45 (30) = happyGoto action_40
action_45 (38) = happyGoto action_41
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (39) = happyShift action_42
action_46 (49) = happyShift action_43
action_46 (50) = happyShift action_44
action_46 (53) = happyShift action_45
action_46 (56) = happyShift action_46
action_46 (59) = happyShift action_47
action_46 (66) = happyShift action_48
action_46 (67) = happyShift action_20
action_46 (68) = happyShift action_15
action_46 (5) = happyGoto action_30
action_46 (6) = happyGoto action_31
action_46 (7) = happyGoto action_32
action_46 (22) = happyGoto action_33
action_46 (23) = happyGoto action_34
action_46 (24) = happyGoto action_35
action_46 (25) = happyGoto action_36
action_46 (26) = happyGoto action_37
action_46 (27) = happyGoto action_53
action_46 (29) = happyGoto action_39
action_46 (30) = happyGoto action_40
action_46 (38) = happyGoto action_41
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (62) = happyShift action_52
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_2

action_49 (47) = happyShift action_51
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_65

action_51 (39) = happyShift action_42
action_51 (49) = happyShift action_43
action_51 (50) = happyShift action_44
action_51 (53) = happyShift action_45
action_51 (56) = happyShift action_46
action_51 (59) = happyShift action_47
action_51 (66) = happyShift action_48
action_51 (67) = happyShift action_20
action_51 (68) = happyShift action_15
action_51 (5) = happyGoto action_30
action_51 (6) = happyGoto action_31
action_51 (7) = happyGoto action_32
action_51 (22) = happyGoto action_33
action_51 (23) = happyGoto action_34
action_51 (24) = happyGoto action_35
action_51 (25) = happyGoto action_36
action_51 (26) = happyGoto action_37
action_51 (27) = happyGoto action_80
action_51 (29) = happyGoto action_39
action_51 (30) = happyGoto action_40
action_51 (38) = happyGoto action_41
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (67) = happyShift action_20
action_52 (6) = happyGoto action_79
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (61) = happyShift action_78
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (60) = happyShift action_77
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (45) = happyShift action_76
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (43) = happyShift action_75
action_56 _ = happyReduce_53

action_57 (51) = happyShift action_74
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (40) = happyShift action_73
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (39) = happyShift action_42
action_59 (66) = happyShift action_48
action_59 (67) = happyShift action_20
action_59 (68) = happyShift action_15
action_59 (5) = happyGoto action_30
action_59 (6) = happyGoto action_31
action_59 (7) = happyGoto action_32
action_59 (22) = happyGoto action_33
action_59 (23) = happyGoto action_34
action_59 (24) = happyGoto action_35
action_59 (25) = happyGoto action_72
action_59 (30) = happyGoto action_40
action_59 (38) = happyGoto action_41
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (39) = happyShift action_42
action_60 (66) = happyShift action_48
action_60 (67) = happyShift action_20
action_60 (68) = happyShift action_15
action_60 (5) = happyGoto action_30
action_60 (6) = happyGoto action_31
action_60 (7) = happyGoto action_32
action_60 (22) = happyGoto action_33
action_60 (23) = happyGoto action_34
action_60 (24) = happyGoto action_71
action_60 (30) = happyGoto action_40
action_60 (38) = happyGoto action_41
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (39) = happyShift action_42
action_61 (66) = happyShift action_48
action_61 (67) = happyShift action_20
action_61 (68) = happyShift action_15
action_61 (5) = happyGoto action_30
action_61 (6) = happyGoto action_31
action_61 (7) = happyGoto action_32
action_61 (22) = happyGoto action_33
action_61 (23) = happyGoto action_34
action_61 (24) = happyGoto action_70
action_61 (30) = happyGoto action_40
action_61 (38) = happyGoto action_41
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (39) = happyShift action_42
action_62 (66) = happyShift action_48
action_62 (67) = happyShift action_20
action_62 (68) = happyShift action_15
action_62 (5) = happyGoto action_30
action_62 (6) = happyGoto action_31
action_62 (7) = happyGoto action_32
action_62 (22) = happyGoto action_33
action_62 (23) = happyGoto action_34
action_62 (30) = happyGoto action_69
action_62 (38) = happyGoto action_41
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_37

action_64 (14) = happyGoto action_68
action_64 _ = happyReduce_18

action_65 (63) = happyShift action_67
action_65 _ = happyReduce_67

action_66 _ = happyReduce_57

action_67 (68) = happyShift action_15
action_67 (7) = happyGoto action_64
action_67 (36) = happyGoto action_65
action_67 (37) = happyGoto action_92
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (39) = happyShift action_91
action_68 (67) = happyShift action_20
action_68 (68) = happyShift action_15
action_68 (6) = happyGoto action_86
action_68 (7) = happyGoto action_87
action_68 (13) = happyGoto action_88
action_68 (15) = happyGoto action_89
action_68 (16) = happyGoto action_90
action_68 _ = happyReduce_66

action_69 _ = happyReduce_39

action_70 (41) = happyShift action_62
action_70 _ = happyReduce_42

action_71 (41) = happyShift action_62
action_71 _ = happyReduce_41

action_72 (42) = happyShift action_60
action_72 (44) = happyShift action_61
action_72 _ = happyReduce_44

action_73 _ = happyReduce_36

action_74 _ = happyReduce_49

action_75 (39) = happyShift action_42
action_75 (49) = happyShift action_43
action_75 (50) = happyShift action_44
action_75 (53) = happyShift action_45
action_75 (56) = happyShift action_46
action_75 (59) = happyShift action_47
action_75 (66) = happyShift action_48
action_75 (67) = happyShift action_20
action_75 (68) = happyShift action_15
action_75 (5) = happyGoto action_30
action_75 (6) = happyGoto action_31
action_75 (7) = happyGoto action_32
action_75 (22) = happyGoto action_33
action_75 (23) = happyGoto action_34
action_75 (24) = happyGoto action_35
action_75 (25) = happyGoto action_36
action_75 (26) = happyGoto action_37
action_75 (27) = happyGoto action_56
action_75 (28) = happyGoto action_85
action_75 (29) = happyGoto action_39
action_75 (30) = happyGoto action_40
action_75 (38) = happyGoto action_41
action_75 _ = happyReduce_52

action_76 (39) = happyShift action_42
action_76 (49) = happyShift action_43
action_76 (50) = happyShift action_44
action_76 (53) = happyShift action_45
action_76 (56) = happyShift action_46
action_76 (59) = happyShift action_47
action_76 (66) = happyShift action_48
action_76 (67) = happyShift action_20
action_76 (68) = happyShift action_15
action_76 (5) = happyGoto action_30
action_76 (6) = happyGoto action_31
action_76 (7) = happyGoto action_32
action_76 (22) = happyGoto action_33
action_76 (23) = happyGoto action_34
action_76 (24) = happyGoto action_35
action_76 (25) = happyGoto action_36
action_76 (26) = happyGoto action_37
action_76 (27) = happyGoto action_84
action_76 (29) = happyGoto action_39
action_76 (30) = happyGoto action_40
action_76 (38) = happyGoto action_41
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (62) = happyShift action_83
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (39) = happyShift action_42
action_78 (49) = happyShift action_43
action_78 (50) = happyShift action_44
action_78 (53) = happyShift action_45
action_78 (56) = happyShift action_46
action_78 (59) = happyShift action_47
action_78 (66) = happyShift action_48
action_78 (67) = happyShift action_20
action_78 (68) = happyShift action_15
action_78 (5) = happyGoto action_30
action_78 (6) = happyGoto action_31
action_78 (7) = happyGoto action_32
action_78 (22) = happyGoto action_33
action_78 (23) = happyGoto action_34
action_78 (24) = happyGoto action_35
action_78 (25) = happyGoto action_36
action_78 (26) = happyGoto action_37
action_78 (27) = happyGoto action_82
action_78 (29) = happyGoto action_39
action_78 (30) = happyGoto action_40
action_78 (38) = happyGoto action_41
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (47) = happyShift action_81
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_61

action_81 (39) = happyShift action_42
action_81 (49) = happyShift action_43
action_81 (50) = happyShift action_44
action_81 (53) = happyShift action_45
action_81 (56) = happyShift action_46
action_81 (59) = happyShift action_47
action_81 (66) = happyShift action_48
action_81 (67) = happyShift action_20
action_81 (68) = happyShift action_15
action_81 (5) = happyGoto action_30
action_81 (6) = happyGoto action_31
action_81 (7) = happyGoto action_32
action_81 (22) = happyGoto action_33
action_81 (23) = happyGoto action_34
action_81 (24) = happyGoto action_35
action_81 (25) = happyGoto action_36
action_81 (26) = happyGoto action_37
action_81 (27) = happyGoto action_106
action_81 (29) = happyGoto action_39
action_81 (30) = happyGoto action_40
action_81 (38) = happyGoto action_41
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (55) = happyShift action_105
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (39) = happyShift action_103
action_83 (52) = happyShift action_104
action_83 (66) = happyShift action_48
action_83 (67) = happyShift action_20
action_83 (68) = happyShift action_15
action_83 (5) = happyGoto action_30
action_83 (6) = happyGoto action_96
action_83 (7) = happyGoto action_97
action_83 (17) = happyGoto action_98
action_83 (18) = happyGoto action_99
action_83 (20) = happyGoto action_100
action_83 (21) = happyGoto action_101
action_83 (38) = happyGoto action_102
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_48

action_85 _ = happyReduce_54

action_86 _ = happyReduce_14

action_87 (14) = happyGoto action_95
action_87 _ = happyReduce_18

action_88 _ = happyReduce_20

action_89 _ = happyReduce_19

action_90 (45) = happyShift action_94
action_90 _ = happyReduce_17

action_91 (39) = happyShift action_91
action_91 (67) = happyShift action_20
action_91 (68) = happyShift action_15
action_91 (6) = happyGoto action_86
action_91 (7) = happyGoto action_87
action_91 (13) = happyGoto action_88
action_91 (15) = happyGoto action_93
action_91 (16) = happyGoto action_90
action_91 _ = happyFail (happyExpListPerState 91)

action_92 _ = happyReduce_68

action_93 (40) = happyShift action_115
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (39) = happyShift action_91
action_94 (67) = happyShift action_20
action_94 (68) = happyShift action_15
action_94 (6) = happyGoto action_86
action_94 (7) = happyGoto action_87
action_94 (13) = happyGoto action_114
action_94 (16) = happyGoto action_90
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (39) = happyShift action_91
action_95 (67) = happyShift action_20
action_95 (68) = happyShift action_15
action_95 (6) = happyGoto action_86
action_95 (7) = happyGoto action_87
action_95 (13) = happyGoto action_88
action_95 (15) = happyGoto action_89
action_95 (16) = happyGoto action_90
action_95 _ = happyReduce_15

action_96 _ = happyReduce_22

action_97 (19) = happyGoto action_113
action_97 _ = happyReduce_28

action_98 _ = happyReduce_27

action_99 (45) = happyShift action_112
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (46) = happyShift action_111
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (64) = happyShift action_110
action_101 _ = happyFail (happyExpListPerState 101)

action_102 _ = happyReduce_25

action_103 (39) = happyShift action_103
action_103 (52) = happyShift action_104
action_103 (66) = happyShift action_48
action_103 (67) = happyShift action_20
action_103 (68) = happyShift action_15
action_103 (5) = happyGoto action_30
action_103 (6) = happyGoto action_96
action_103 (7) = happyGoto action_97
action_103 (17) = happyGoto action_98
action_103 (18) = happyGoto action_109
action_103 (38) = happyGoto action_102
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_26

action_105 (39) = happyShift action_42
action_105 (49) = happyShift action_43
action_105 (50) = happyShift action_44
action_105 (53) = happyShift action_45
action_105 (56) = happyShift action_46
action_105 (59) = happyShift action_47
action_105 (66) = happyShift action_48
action_105 (67) = happyShift action_20
action_105 (68) = happyShift action_15
action_105 (5) = happyGoto action_30
action_105 (6) = happyGoto action_31
action_105 (7) = happyGoto action_32
action_105 (22) = happyGoto action_33
action_105 (23) = happyGoto action_34
action_105 (24) = happyGoto action_35
action_105 (25) = happyGoto action_36
action_105 (26) = happyGoto action_37
action_105 (27) = happyGoto action_108
action_105 (29) = happyGoto action_39
action_105 (30) = happyGoto action_40
action_105 (38) = happyGoto action_41
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (64) = happyShift action_107
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (58) = happyShift action_120
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_46

action_109 (40) = happyShift action_119
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_50

action_111 (39) = happyShift action_103
action_111 (52) = happyShift action_104
action_111 (66) = happyShift action_48
action_111 (67) = happyShift action_20
action_111 (68) = happyShift action_15
action_111 (5) = happyGoto action_30
action_111 (6) = happyGoto action_96
action_111 (7) = happyGoto action_97
action_111 (17) = happyGoto action_98
action_111 (18) = happyGoto action_99
action_111 (20) = happyGoto action_100
action_111 (21) = happyGoto action_118
action_111 (38) = happyGoto action_102
action_111 _ = happyReduce_31

action_112 (39) = happyShift action_42
action_112 (49) = happyShift action_43
action_112 (50) = happyShift action_44
action_112 (53) = happyShift action_45
action_112 (56) = happyShift action_46
action_112 (59) = happyShift action_47
action_112 (66) = happyShift action_48
action_112 (67) = happyShift action_20
action_112 (68) = happyShift action_15
action_112 (5) = happyGoto action_30
action_112 (6) = happyGoto action_31
action_112 (7) = happyGoto action_32
action_112 (22) = happyGoto action_33
action_112 (23) = happyGoto action_34
action_112 (24) = happyGoto action_35
action_112 (25) = happyGoto action_36
action_112 (26) = happyGoto action_37
action_112 (27) = happyGoto action_117
action_112 (29) = happyGoto action_39
action_112 (30) = happyGoto action_40
action_112 (38) = happyGoto action_41
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (39) = happyShift action_103
action_113 (52) = happyShift action_104
action_113 (66) = happyShift action_48
action_113 (67) = happyShift action_20
action_113 (68) = happyShift action_15
action_113 (5) = happyGoto action_30
action_113 (6) = happyGoto action_96
action_113 (7) = happyGoto action_97
action_113 (17) = happyGoto action_98
action_113 (18) = happyGoto action_116
action_113 (38) = happyGoto action_102
action_113 _ = happyReduce_24

action_114 _ = happyReduce_16

action_115 _ = happyReduce_21

action_116 _ = happyReduce_29

action_117 _ = happyReduce_30

action_118 _ = happyReduce_32

action_119 _ = happyReduce_23

action_120 (39) = happyShift action_42
action_120 (49) = happyShift action_43
action_120 (50) = happyShift action_44
action_120 (53) = happyShift action_45
action_120 (56) = happyShift action_46
action_120 (59) = happyShift action_47
action_120 (66) = happyShift action_48
action_120 (67) = happyShift action_20
action_120 (68) = happyShift action_15
action_120 (5) = happyGoto action_30
action_120 (6) = happyGoto action_31
action_120 (7) = happyGoto action_32
action_120 (22) = happyGoto action_33
action_120 (23) = happyGoto action_34
action_120 (24) = happyGoto action_35
action_120 (25) = happyGoto action_36
action_120 (26) = happyGoto action_37
action_120 (27) = happyGoto action_121
action_120 (29) = happyGoto action_39
action_120 (30) = happyGoto action_40
action_120 (38) = happyGoto action_41
action_120 _ = happyFail (happyExpListPerState 120)

action_121 _ = happyReduce_47

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn5
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (T_LowerIdent happy_var_1)))
	 =  HappyAbsSyn6
		 (LowerIdent (happy_var_1)
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal (PT _ (T_UpperIdent happy_var_1)))
	 =  HappyAbsSyn7
		 (UpperIdent (happy_var_1)
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  8 happyReduction_5
happyReduction_5  =  HappyAbsSyn8
		 ([]
	)

happyReduce_6 = happySpecReduce_2  8 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn8
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  9 happyReduction_7
happyReduction_7  =  HappyAbsSyn9
		 ([]
	)

happyReduce_8 = happySpecReduce_2  9 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn9
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  10 happyReduction_9
happyReduction_9 (HappyAbsSyn34  happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn10
		 (AbsGrammar.Program happy_var_1 (reverse happy_var_2) (reverse happy_var_3)
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  11 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (AbsGrammar.Import happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_0  12 happyReduction_11
happyReduction_11  =  HappyAbsSyn12
		 ([]
	)

happyReduce_12 = happySpecReduce_1  12 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ((:[]) happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  12 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  13 happyReduction_14
happyReduction_14 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn13
		 (AbsGrammar.TVar happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  13 happyReduction_15
happyReduction_15 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn13
		 (AbsGrammar.TADT happy_var_1 (reverse happy_var_2)
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  13 happyReduction_16
happyReduction_16 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (AbsGrammar.TArr happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  14 happyReduction_18
happyReduction_18  =  HappyAbsSyn14
		 ([]
	)

happyReduce_19 = happySpecReduce_2  14 happyReduction_19
happyReduction_19 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  15 happyReduction_20
happyReduction_20 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  16 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  17 happyReduction_22
happyReduction_22 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn17
		 (AbsGrammar.PVar happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  17 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  18 happyReduction_24
happyReduction_24 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn17
		 (AbsGrammar.PCon happy_var_1 (reverse happy_var_2)
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  18 happyReduction_25
happyReduction_25 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn17
		 (AbsGrammar.PLit happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  18 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn17
		 (AbsGrammar.PAny
	)

happyReduce_27 = happySpecReduce_1  18 happyReduction_27
happyReduction_27 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_0  19 happyReduction_28
happyReduction_28  =  HappyAbsSyn19
		 ([]
	)

happyReduce_29 = happySpecReduce_2  19 happyReduction_29
happyReduction_29 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  20 happyReduction_30
happyReduction_30 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn20
		 (AbsGrammar.Branch happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  21 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn21
		 ((:[]) happy_var_1
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  21 happyReduction_32
happyReduction_32 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn21
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  22 happyReduction_33
happyReduction_33 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn22
		 (AbsGrammar.EVar happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  22 happyReduction_34
happyReduction_34 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn22
		 (AbsGrammar.ECon happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  22 happyReduction_35
happyReduction_35 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn22
		 (AbsGrammar.ELit happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  22 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  23 happyReduction_37
happyReduction_37 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (AbsGrammar.EApp happy_var_1 happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  23 happyReduction_38
happyReduction_38 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  24 happyReduction_39
happyReduction_39 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (AbsGrammar.EMul happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  24 happyReduction_40
happyReduction_40 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  25 happyReduction_41
happyReduction_41 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (AbsGrammar.EAdd happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  25 happyReduction_42
happyReduction_42 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (AbsGrammar.ESub happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  25 happyReduction_43
happyReduction_43 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  26 happyReduction_44
happyReduction_44 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (AbsGrammar.EEq happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  26 happyReduction_45
happyReduction_45 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happyReduce 6 27 happyReduction_46
happyReduction_46 ((HappyAbsSyn22  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (AbsGrammar.EIf happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_47 = happyReduce 8 27 happyReduction_47
happyReduction_47 ((HappyAbsSyn22  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (AbsGrammar.ELet happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_48 = happyReduce 4 27 happyReduction_48
happyReduction_48 ((HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (AbsGrammar.ELam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_49 = happySpecReduce_3  27 happyReduction_49
happyReduction_49 _
	(HappyAbsSyn28  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (AbsGrammar.ELst happy_var_2
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happyReduce 6 27 happyReduction_50
happyReduction_50 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (AbsGrammar.ECase happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_1  27 happyReduction_51
happyReduction_51 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_0  28 happyReduction_52
happyReduction_52  =  HappyAbsSyn28
		 ([]
	)

happyReduce_53 = happySpecReduce_1  28 happyReduction_53
happyReduction_53 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn28
		 ((:[]) happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  28 happyReduction_54
happyReduction_54 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn28
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  29 happyReduction_55
happyReduction_55 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  30 happyReduction_56
happyReduction_56 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happyReduce 5 31 happyReduction_57
happyReduction_57 ((HappyAbsSyn37  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn31
		 (AbsGrammar.DataDecl happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_58 = happySpecReduce_0  32 happyReduction_58
happyReduction_58  =  HappyAbsSyn32
		 ([]
	)

happyReduce_59 = happySpecReduce_3  32 happyReduction_59
happyReduction_59 _
	(HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  33 happyReduction_60
happyReduction_60 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn33
		 (AbsGrammar.AssignDecl happy_var_1 happy_var_3
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 5 33 happyReduction_61
happyReduction_61 ((HappyAbsSyn22  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (AbsGrammar.FunDecl happy_var_1 happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_62 = happySpecReduce_1  33 happyReduction_62
happyReduction_62 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_0  34 happyReduction_63
happyReduction_63  =  HappyAbsSyn34
		 ([]
	)

happyReduce_64 = happySpecReduce_3  34 happyReduction_64
happyReduction_64 _
	(HappyAbsSyn33  happy_var_2)
	(HappyAbsSyn34  happy_var_1)
	 =  HappyAbsSyn34
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  35 happyReduction_65
happyReduction_65 _
	(HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (happy_var_2
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_2  36 happyReduction_66
happyReduction_66 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn36
		 (AbsGrammar.Constr happy_var_1 (reverse happy_var_2)
	)
happyReduction_66 _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  37 happyReduction_67
happyReduction_67 (HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn37
		 ((:[]) happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  37 happyReduction_68
happyReduction_68 (HappyAbsSyn37  happy_var_3)
	_
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn37
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  38 happyReduction_69
happyReduction_69 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn38
		 (AbsGrammar.LInt happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 69 69 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 39;
	PT _ (TS _ 2) -> cont 40;
	PT _ (TS _ 3) -> cont 41;
	PT _ (TS _ 4) -> cont 42;
	PT _ (TS _ 5) -> cont 43;
	PT _ (TS _ 6) -> cont 44;
	PT _ (TS _ 7) -> cont 45;
	PT _ (TS _ 8) -> cont 46;
	PT _ (TS _ 9) -> cont 47;
	PT _ (TS _ 10) -> cont 48;
	PT _ (TS _ 11) -> cont 49;
	PT _ (TS _ 12) -> cont 50;
	PT _ (TS _ 13) -> cont 51;
	PT _ (TS _ 14) -> cont 52;
	PT _ (TS _ 15) -> cont 53;
	PT _ (TS _ 16) -> cont 54;
	PT _ (TS _ 17) -> cont 55;
	PT _ (TS _ 18) -> cont 56;
	PT _ (TS _ 19) -> cont 57;
	PT _ (TS _ 20) -> cont 58;
	PT _ (TS _ 21) -> cont 59;
	PT _ (TS _ 22) -> cont 60;
	PT _ (TS _ 23) -> cont 61;
	PT _ (TS _ 24) -> cont 62;
	PT _ (TS _ 25) -> cont 63;
	PT _ (TS _ 26) -> cont 64;
	PT _ (TL happy_dollar_dollar) -> cont 65;
	PT _ (TI happy_dollar_dollar) -> cont 66;
	PT _ (T_LowerIdent happy_dollar_dollar) -> cont 67;
	PT _ (T_UpperIdent happy_dollar_dollar) -> cont 68;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 69 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn10 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

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

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






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
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
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
