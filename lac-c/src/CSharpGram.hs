module CSharpGram where

import Prelude hiding ((<*), (<$), (*>))
import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex


data Class = Class Token [Member]
    deriving Show

data Member = MemberD Decl
            | MemberM Type Token [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConst  Token
          | ExprVar    Token
          | ExprOper   Token Expr Expr
          | ExprArg    Token [Expr]
          | ExprOperU  Token Expr
          deriving Show

data Decl = Decl Type Token
    deriving Show

data Type = TypeVoid
          | TypePrim  Token
          | TypeObj   Token
          | TypeArray Type
          deriving (Eq,Show)


parenthesised p = pack (symbol POpen) p (symbol PClose)
bracketed     p = pack (symbol SOpen) p (symbol SClose)
braced        p = pack (symbol COpen) p (symbol CClose)

pExprSimple :: Parser Token Expr
pExprSimple =  ExprConst <$> sConst
           <|> ExprVar   <$> sLowerId
           <|> pOperU
           <|> parenthesised pExpr
           <|> ExprArg   <$> sLowerId <*> argList
             where argList = parenthesised (option (listOf pExpr (symbol Comma)) [])

pExprMul, pExprAdd, pExprRel, pExprEql, pExprExc, pExprAnd, pExprOr, pExpr :: Parser Token Expr
pExprMul = chainl pExprSimple (op "*"  <|> op "/"  <|> op "%")
pExprAdd = chainl pExprMul    (op "+"  <|> op "-")
pExprRel = chainl pExprAdd    (op "<=" <|> op "<" <|> op ">=" <|> op ">")
pExprEql = chainl pExprRel    (op "==" <|> op "!=")
pExprExc = chainl pExprEql    (op "^")
pExprAnd = chainl pExprExc    (op "&&")
pExprOr  = chainl pExprAnd    (op "||")
pExpr    = chainr pExprOr     (op "=" <|> op "+=")

op :: String -> Parser Token (Expr -> Expr -> Expr)
op s = ExprOper <$> symbol (Operator s)

pOperU :: Parser Token Expr
pOperU = (flip ExprOperU) <$> (ExprVar <$> sLowerId) <*> (symbol (Operator "++"))

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi <|> pMeth

pStatDecl :: Parser Token Stat
pStatDecl =  pStat <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr   <$> pExpr <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr <*> pStat
     <|> StatReturn <$ symbol KeyReturn <*> pExpr               <*  sSemi
     <|> pBlock
     where optionalElse = option ((\_ x -> x) <$> symbol KeyElse <*> pStat) (StatBlock [])

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> (const TypeVoid <$> symbol KeyVoid)
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pType0 :: Parser Token Type
pType0 =  TypePrim <$> sStdType <|> TypeObj <$> sUpperId

pType :: Parser Token Type
pType = foldr (const TypeArray) <$> pType0 <*> many (bracketed (succeed ()))

pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = const <$> pDecl <*> sSemi

pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

