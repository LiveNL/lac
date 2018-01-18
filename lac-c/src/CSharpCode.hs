module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import Data.Map as M
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM

import Data.Char

data ValueOrAddress = Value | Address
    deriving Show

type Env = M.Map String Int

codeAlgebra :: CSharpAlgebra Code Code (Env -> Code) (ValueOrAddress -> Env -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fExprArg)
    )

fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> Code
fMembDecl d = []

fMembMeth :: Type -> Token -> [Decl] -> (Env -> Code) -> Code
fMembMeth t (LowerId x) ps s = [LABEL x] ++ [LINK $ length ps] ++ s (toEnv M.empty ps) ++ [UNLINK] ++ [RET]

toEnv :: Env -> [Decl] -> Env
toEnv env ps = fst $ Prelude.foldr insertArgs (env, - 2) ps

insertArgs :: Decl -> (Env, Int) -> (Env, Int)
insertArgs d (env, i) = (M.insert (key d) i env, i - 1)

key :: Decl -> String
key (Decl _ (LowerId x)) = x

fStatDecl :: Decl -> Env -> Code
fStatDecl d env = []

fStatExpr :: (ValueOrAddress -> Env -> Code) -> Env -> Code
fStatExpr e env = e Value env ++ [pop]

fStatIf :: (ValueOrAddress -> Env -> Code) -> (Env -> Code) -> (Env -> Code) -> Env -> Code
fStatIf e s1 s2 env = c ++ [BRF (n1 + 2)] ++ (s1 env) ++ [BRA n2] ++ (s2 env)
    where
        c        = e Value env
        (n1, n2) = (codeSize (s1 env), codeSize (s2 env))

fStatWhile :: (ValueOrAddress -> Env -> Code) -> (Env -> Code) -> Env -> Code
fStatWhile e s1 env = [BRA n] ++ (s1 env) ++ c ++ [BRT (-(n + k + 2))]
    where
      c = e Value env
      (n, k) = (codeSize (s1 env), codeSize c)

fStatReturn :: (ValueOrAddress -> Env -> Code) -> Env -> Code
fStatReturn e env = e Value env ++ [STR r3] ++ [pop] ++ [RET]

fStatBlock :: [Env -> Code] -> Env -> Code
fStatBlock x env = concatMap ($ env) x

fExprCon :: Token -> ValueOrAddress -> Env -> Code
fExprCon (ConstInt n)  va env = [LDC n]
fExprCon (ConstChar n) va env = [LDC (digitToInt n)]
fExprCon (ConstBool n) va env = [LDC (x n)]
  where x n = if n then 1 else 0

fExprVar :: Token -> (ValueOrAddress -> Env -> Code)
fExprVar (LowerId x) va env = let loc = env M.! x in case va of
                                              Value    ->  [LDL  loc]
                                              Address  ->  [LDLA loc]

fExprOp :: Token -> (ValueOrAddress -> Env -> Code) -> (ValueOrAddress -> Env -> Code) -> ValueOrAddress -> Env -> Code
fExprOp (Operator "=") e1 e2 va env = e2 Value env ++ [LDS 0] ++ e1 Address env ++ [STA 0]
fExprOp (Operator op)  e1 e2 va env = e1 Value env ++ e2 Value env ++ [opCodes ! op]

fExprArg :: Token -> [ValueOrAddress -> Env -> Code] -> ValueOrAddress -> Env -> Code
fExprArg (LowerId "print") es va env = concat [e Value env | e <- es] ++ [TRAP 0]
fExprArg (LowerId x)       es va env = concat [e Value env | e <- es] ++ [Bsr x] ++ [LDR r3]

opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

