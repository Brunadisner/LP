{-# OPTIONS_GHC -w #-}
module Parser where 

import Lexer
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,31) ([912,2,12288,0,0,228,36899,29187,0,0,49209,58384,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","Exp","num","'+'","'&'","true","false","if","then","else","%eof"]
        bit_start = st * 13
        bit_end = (st + 1) * 13
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..12]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (5) = happyShift action_2
action_0 (8) = happyShift action_4
action_0 (9) = happyShift action_5
action_0 (10) = happyShift action_6
action_0 (4) = happyGoto action_3
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (5) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (6) = happyShift action_8
action_3 (7) = happyShift action_9
action_3 (13) = happyAccept
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 _ = happyReduce_2

action_6 (5) = happyShift action_2
action_6 (8) = happyShift action_4
action_6 (9) = happyShift action_5
action_6 (10) = happyShift action_6
action_6 (4) = happyGoto action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (6) = happyShift action_8
action_7 (7) = happyShift action_9
action_7 (11) = happyShift action_12
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (5) = happyShift action_2
action_8 (8) = happyShift action_4
action_8 (9) = happyShift action_5
action_8 (10) = happyShift action_6
action_8 (4) = happyGoto action_11
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (5) = happyShift action_2
action_9 (8) = happyShift action_4
action_9 (9) = happyShift action_5
action_9 (10) = happyShift action_6
action_9 (4) = happyGoto action_10
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (6) = happyShift action_8
action_10 (7) = happyShift action_9
action_10 _ = happyReduce_5

action_11 (6) = happyShift action_8
action_11 (7) = happyShift action_9
action_11 _ = happyReduce_4

action_12 (5) = happyShift action_2
action_12 (8) = happyShift action_4
action_12 (9) = happyShift action_5
action_12 (10) = happyShift action_6
action_12 (4) = happyGoto action_13
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (6) = happyShift action_8
action_13 (7) = happyShift action_9
action_13 (12) = happyShift action_14
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (5) = happyShift action_2
action_14 (8) = happyShift action_4
action_14 (9) = happyShift action_5
action_14 (10) = happyShift action_6
action_14 (4) = happyGoto action_15
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (6) = happyShift action_8
action_15 (7) = happyShift action_9
action_15 _ = happyReduce_6

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenNum happy_var_1))
	 =  HappyAbsSyn4
		 (Num happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 _
	 =  HappyAbsSyn4
		 (BFalse
	)

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn4
		 (BTrue
	)

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Add happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 6 4 happyReduction_6
happyReduction_6 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 13 13 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenNum happy_dollar_dollar -> cont 5;
	TokenAdd -> cont 6;
	TokenAnd -> cont 7;
	TokenTrue -> cont 8;
	TokenFalse -> cont 9;
	TokenIf -> cont 10;
	TokenThen -> cont 11;
	TokenElse -> cont 12;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 13 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a 
parseError _ = error "Syntax error!"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $









