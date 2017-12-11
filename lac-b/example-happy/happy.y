{
module Main where
}

%name kevin
%tokentype { Token }
%error { parseError }

%token
      let    { Let    }
      in     { In     }
      sym    { Sym $$ }
      va     { Var $$ }
      digit  { Int $$ }
%%

Tekst   : let va digit in sym  { Leta $2 $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Tekst  
      = Leta String Int
      deriving Show

data Token 
      = Let
      | In
      | Sym Char
      | Var String
      | Int Int
  deriving (Eq,Show)

main = undefined
}