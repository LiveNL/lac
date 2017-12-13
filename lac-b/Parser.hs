{-# OPTIONS_GHC -w #-}
module Parser where
import Data.List
import Debug.Trace
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12

action_0 (34) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 _ = happyFail

action_1 (34) = happyShift action_4
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 _ = happyFail

action_2 (34) = happyShift action_4
action_2 (6) = happyGoto action_7
action_2 _ = happyReduce_1

action_3 _ = happyReduce_2

action_4 (13) = happyShift action_6
action_4 _ = happyFail

action_5 (38) = happyAccept
action_5 _ = happyFail

action_6 (16) = happyShift action_10
action_6 (17) = happyShift action_11
action_6 (18) = happyShift action_12
action_6 (19) = happyShift action_13
action_6 (20) = happyShift action_14
action_6 (21) = happyShift action_15
action_6 (34) = happyShift action_16
action_6 (7) = happyGoto action_8
action_6 (8) = happyGoto action_9
action_6 _ = happyFail

action_7 _ = happyReduce_3

action_8 (14) = happyShift action_22
action_8 (15) = happyShift action_23
action_8 _ = happyFail

action_9 _ = happyReduce_5

action_10 _ = happyReduce_7

action_11 _ = happyReduce_8

action_12 _ = happyReduce_9

action_13 _ = happyReduce_10

action_14 (24) = happyShift action_18
action_14 (25) = happyShift action_19
action_14 (26) = happyShift action_20
action_14 (12) = happyGoto action_21
action_14 _ = happyFail

action_15 (24) = happyShift action_18
action_15 (25) = happyShift action_19
action_15 (26) = happyShift action_20
action_15 (12) = happyGoto action_17
action_15 _ = happyFail

action_16 _ = happyReduce_13

action_17 (22) = happyShift action_25
action_17 _ = happyFail

action_18 _ = happyReduce_24

action_19 _ = happyReduce_23

action_20 _ = happyReduce_25

action_21 _ = happyReduce_11

action_22 _ = happyReduce_4

action_23 (16) = happyShift action_10
action_23 (17) = happyShift action_11
action_23 (18) = happyShift action_12
action_23 (19) = happyShift action_13
action_23 (20) = happyShift action_14
action_23 (21) = happyShift action_15
action_23 (34) = happyShift action_16
action_23 (8) = happyGoto action_24
action_23 _ = happyFail

action_24 _ = happyReduce_6

action_25 (28) = happyShift action_29
action_25 (29) = happyShift action_30
action_25 (30) = happyShift action_31
action_25 (31) = happyShift action_32
action_25 (32) = happyShift action_33
action_25 (33) = happyShift action_34
action_25 (9) = happyGoto action_26
action_25 (10) = happyGoto action_27
action_25 (11) = happyGoto action_28
action_25 _ = happyFail

action_26 (23) = happyShift action_36
action_26 (27) = happyShift action_37
action_26 _ = happyFail

action_27 _ = happyReduce_14

action_28 (13) = happyShift action_35
action_28 _ = happyFail

action_29 _ = happyReduce_17

action_30 _ = happyReduce_18

action_31 _ = happyReduce_19

action_32 _ = happyReduce_20

action_33 _ = happyReduce_21

action_34 _ = happyReduce_22

action_35 (16) = happyShift action_10
action_35 (17) = happyShift action_11
action_35 (18) = happyShift action_12
action_35 (19) = happyShift action_13
action_35 (20) = happyShift action_14
action_35 (21) = happyShift action_15
action_35 (34) = happyShift action_16
action_35 (7) = happyGoto action_39
action_35 (8) = happyGoto action_9
action_35 _ = happyFail

action_36 _ = happyReduce_12

action_37 (28) = happyShift action_29
action_37 (29) = happyShift action_30
action_37 (30) = happyShift action_31
action_37 (31) = happyShift action_32
action_37 (32) = happyShift action_33
action_37 (33) = happyShift action_34
action_37 (10) = happyGoto action_38
action_37 (11) = happyGoto action_28
action_37 _ = happyFail

action_38 _ = happyReduce_15

action_39 (15) = happyShift action_23
action_39 _ = happyReduce_16

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_2 : happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 6 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TLetter happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Rule happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_3 : happy_var_1
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn8
		 (Go
	)

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn8
		 (Take
	)

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn8
		 (Mark
	)

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn8
		 (Parser.Nothing
	)

happyReduce_11 = happySpecReduce_2  8 happyReduction_11
happyReduction_11 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Turn happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 5 8 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Case happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyTerminal (TLetter happy_var_1))
	 =  HappyAbsSyn8
		 (Next happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_3 : happy_var_1
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  10 happyReduction_16
happyReduction_16 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (Alt happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 _
	 =  HappyAbsSyn11
		 (Empty
	)

happyReduce_18 = happySpecReduce_1  11 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn11
		 (Lambda
	)

happyReduce_19 = happySpecReduce_1  11 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn11
		 (Debris
	)

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn11
		 (Asteroid
	)

happyReduce_21 = happySpecReduce_1  11 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn11
		 (Boundary
	)

happyReduce_22 = happySpecReduce_1  11 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn11
		 (Rest
	)

happyReduce_23 = happySpecReduce_1  12 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn12
		 (Parser.Right
	)

happyReduce_24 = happySpecReduce_1  12 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn12
		 (Parser.Left
	)

happyReduce_25 = happySpecReduce_1  12 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn12
		 (Front
	)

happyNewToken action sts stk [] =
	action 38 38 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TArrow -> cont 13;
	TDot -> cont 14;
	TComma -> cont 15;
	TGo -> cont 16;
	TTake -> cont 17;
	TMark -> cont 18;
	TNothing -> cont 19;
	TTurn -> cont 20;
	TCase -> cont 21;
	TOf -> cont 22;
	TEnd -> cont 23;
	TLeft -> cont 24;
	TRight -> cont 25;
	TFront -> cont 26;
	TSemicolon -> cont 27;
	TEmpty -> cont 28;
	TLambda -> cont 29;
	TDebris -> cont 30;
	TAsteroid -> cont 31;
	TBoundary -> cont 32;
	TUnderscore -> cont 33;
	TLetter happy_dollar_dollar -> cont 34;
	TDigit happy_dollar_dollar -> cont 35;
	TPlus -> cont 36;
	TMinus -> cont 37;
	_ -> happyError' (tk:tks)
	}

happyError_ 38 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
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
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseProgram tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token = TArrow
           | TDot
           | TComma
           | TGo
           | TTake
           | TMark
           | TNothing
           | TTurn
           | TCase
           | TOf
           | TEnd
           | TLeft
           | TRight
           | TFront
           | TSemicolon
           | TEmpty
           | TLambda
           | TDebris
           | TAsteroid
           | TBoundary
           | TUnderscore
           | TLetter String
           | TDigit Int
           | TPlus
           | TMinus
  deriving (Eq, Show)

data Program = Program [Rule]
    deriving Show

data Rule = Rule Ident [Cmd]
    deriving Show

type Ident = String

data Cmd = Go
         | Take
         | Mark
         | Nothing
         | Turn Dir
         | Case Dir [Alt]
         | Next Ident
  deriving Show

data Alt = Alt Pat [Cmd]
    deriving Show

data Pat = Lambda
         | Debris
         | Asteroid
         | Boundary
         | Empty
         | Rest
  deriving (Show, Eq)

data Dir = Right
         | Left
         | Front
  deriving Show

-- main = getContents >>= print . parseProgram
main = undefined

type ProgramAlgebra p r x a = ([r] -> p,         -- program
                              Ident -> [x] -> r, -- rule
                              x,                 -- go
                              x,                 -- take
                              x,                 -- mark
                              x,                 -- nothing
                              Dir -> x,          -- turn
                              Dir -> [a] -> x,   -- case
                              Ident -> x,        -- next
                              Pat -> [x] -> a)   -- alt

foldCmd :: ProgramAlgebra p r x a -> Program -> p
foldCmd (program, rule, go, take, mark, nothing, turn, c, next, alt) = fp
  where fp (Program xs) = program (map fr xs)
        fr (Rule x xs) = rule x (map ff xs)
        ff Go = go
        ff Take = take
        ff Mark = mark
        ff Parser.Nothing = nothing
        ff (Turn x) = turn x
        ff (Case x xs) = c x (map fa xs)
        ff (Next x) = next x
        fa (Alt x xs) = alt x (map ff xs)

-- There are no calls to undefined rules (rules may be used before they are defined though)
notUndef :: ProgramAlgebra Bool (Ident,[Ident]) [Ident] [Ident]
notUndef = ((\xs -> f xs), (\x xs -> (x, (concat xs))), [""], [""], [""], [""], (\x -> [""]), (\_ xs -> concat xs), (\x -> [x]), (\_ x -> concat x))
  where f xs = all (flip elem (defs xs)) (snds xs)
        defs xs = map fst xs
        snds xs = filter (/="") (concat (map snd xs))

-- No rule is defined twice
notTwice :: ProgramAlgebra Bool Ident Bool Bool
notTwice = ((\xs -> (length xs) == (length (nub xs))), (\x _ -> x), False, False, False, False, (\x -> False), (\_ _ -> False), (\x -> False), (\_ _ -> False))

-- There is a rule named start.
hasStart :: ProgramAlgebra Bool Bool Bool Bool
hasStart = ((\xs -> or xs), (\x _ -> r x), False, False, False, False, (\x -> False), (\_ _ -> False), (\x -> False), (\_ _ -> False))
  where r x = x == "start"

-- There is no possibility for pattern match failure, i.e., all case expressions must either contain a catch-all pattern _ or contain cases for all five other options.
allPats :: ProgramAlgebra Bool Bool Bool Pat
allPats = ((\xs -> or xs), (\_ xs -> or xs), False, False, False, False, (\x -> False), (\_ xs -> r xs), (\x -> False), (\x _ -> x))
  where r xs = elem Rest xs || (length (nub xs) == 5) && (all def xs)
        def x = elem x [Lambda, Debris, Asteroid, Boundary, Empty]

-- check (Program (parseProgram [Token]))
check :: Program -> Bool
check p = and [u,t,s,a]
  where u = foldCmd notUndef p
        t = foldCmd notTwice p
        s = foldCmd hasStart p
        a = foldCmd allPats p
{-# LINE 1 "templates/GenericTemplate.hs" #-}



















































































































































































-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

























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
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

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
happyFail  i tk (HappyState (action)) sts stk =
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

