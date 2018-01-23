module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import Data.Map as M
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM

import Data.Char
import Debug.Trace

data ValueOrAddress = Value | Address
    deriving Show

type Env = M.Map String Int
type DCode = (Code, [Decl], Env)

frst :: (a, b, c) -> a
frst (a,_,_) = a

scnd :: (a, b, c) -> b
scnd (_,b,_) = b

thrd :: (a, b, c) -> c
thrd (_,_,c) = c

codeAlgebra :: CSharpAlgebra Code Code (Env -> DCode) (ValueOrAddress -> Env -> Code)
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

fMembMeth :: Type -> Token -> [Decl] -> (Env -> DCode) -> Code
fMembMeth t (LowerId x) ps s = [LABEL x] ++ [LINK l] ++ frst c ++ [UNLINK] ++ [RET]
  where c = (s (toEnv M.empty ps))
        l = (length (scnd c))

toEnv :: Env -> [Decl] -> Env
toEnv env ps = fst $ Prelude.foldr insertArgs (env, - 2) ps

insertArgs :: Decl -> (Env, Int) -> (Env, Int)
insertArgs d (env, i) = (M.insert (key d) i env, i - 1)

key :: Decl -> String
key (Decl _ (LowerId x)) = x

dkey :: String -> Decl
dkey x = (Decl (TypePrim (StdType "int")) (LowerId x))

fStatDecl :: Decl -> Env -> DCode
fStatDecl d env = ([], [d], newEnv)
  where newEnv = M.insert (key d) (i + 1) env
        i = M.size (M.filter (>= 0) env)

fStatExpr :: (ValueOrAddress -> Env -> Code) -> Env -> DCode
fStatExpr e env = (e Value env ++ [pop], [], env)

fStatIf :: (ValueOrAddress -> Env -> Code) -> (Env -> DCode) -> (Env -> DCode) -> Env -> DCode
fStatIf e s1 s2 env = (c ++ [BRF (n1 + 2)] ++ frst (s1 env) ++ [BRA n2] ++ frst (s2 env), [], env)
    where
        c        = e Value env
        (n1, n2) = (codeSize (frst (s1 env)), codeSize (frst (s2 env)))

fStatWhile :: (ValueOrAddress -> Env -> Code) -> (Env -> DCode) -> Env -> DCode
fStatWhile e s1 env = ([BRA n] ++ frst (s1 env) ++ c ++ [BRT (-(n + k + 2))], [], env)
    where
      c = e Value env
      (n, k) = (codeSize (frst (s1 env)), codeSize c)

fStatReturn :: (ValueOrAddress -> Env -> Code) -> Env -> DCode
fStatReturn e env = (e Value env ++ [LDS 0, STR r3, pop, UNLINK, RET], [], env)

fStatBlock :: [Env -> DCode] -> Env -> DCode
fStatBlock xs env = ((frst newDCode), (concatMap decls xs), (thrd newDCode))
  where newDCode = if length xs > 1 then Prelude.foldl foldBlock ((head xs) env) xs         -- TODO: fix lame length check
                                    else ((concatMap code xs), (concatMap decls xs), env)
        decls dcode = scnd (dcode env)
        code  dcode = frst (dcode env)

foldBlock :: DCode -> (Env -> DCode) -> DCode
foldBlock (code, decls, env) e = let (code', decls', env') = e env
                                 in (code ++ code', decls ++ decls', env')

fExprCon :: Token -> ValueOrAddress -> Env -> Code
fExprCon (ConstInt n)  va env = [LDC n]
fExprCon (ConstChar n) va env = [LDC (digitToInt n)]
fExprCon (ConstBool n) va env = [LDC (x n)]
  where x n = if n then 1 else 0

fExprVar :: Token -> ValueOrAddress -> Env -> Code
fExprVar (LowerId x) va env = if member x env then let loc = env M.! x in case va of
                                                        Value    ->  [LDL loc]
                                                        Address  ->  [LDLA loc]
                                              else error ("Missing var: " ++ x ++ ".")

fExprOp :: Token -> (ValueOrAddress -> Env -> Code) -> (ValueOrAddress -> Env -> Code) -> ValueOrAddress -> Env -> Code
fExprOp (Operator "=") e1 e2 va env = e2 Value env ++ [LDS 0] ++ e1 Address env ++ [STA 0]
fExprOp (Operator op) e1 e2 va env = e1 Value env ++ e2 Value env ++ [opCodes ! op]

fExprArg :: Token -> [ValueOrAddress -> Env -> Code] -> ValueOrAddress -> Env -> Code
fExprArg (LowerId "print") es va env = concat [e Value env | e <- es] ++ [TRAP 0]
fExprArg (LowerId x)       es va env = concat [e Value env | e <- es] ++ [Bsr x] ++ [LDR r3]

opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

