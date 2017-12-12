{
module Parser where
}

%name parseProgram
%tokentype { Token }
%error { parseError }

%token
'->'            { TArrow }
'.'             { TDot }
','             { TComma }
go              { TGo }
take            { TTake }
mark            { TMark }
nothing         { TNothing }
turn            { TTurn }
case            { TCase }
of              { TOf }
end             { TEnd }
left            { TLeft }
right           { TRight }
front           { TFront }
';'             { TSemicolon }
empty           { TEmpty }
lamda           { TLambda }
debris          { TDebris }
asteroid        { TAsteroid }
boundary        { TBoundary }
'_'             { TUnderscore }
letter          { TLetter $$ }
int             { TDigit $$ }
'+'             { TPlus }
'-'             { TMinus }

%%

Program : rules                { $1 }

rules   : Rule                 { [$1] }
        | rules Rule           { $2 : $1 }

Rule    : letter '->' cmds '.' { Rule $1 $3 }

cmds    : Cmd                  { [$1] }
        | cmds ',' Cmd         { $3 : $1 }

Cmd     : go                   { Go }
        | take                 { Take }
        | mark                 { Mark }
        | nothing              { Parser.Nothing }
        | turn Dir             { Turn $2 }
        | case Dir of alts end { Case $2 $4 }
        | letter               { Next $1 }

alts    : Alt                  { [$1] }
        | alts ';' Alt         { $3 : $1 }

Alt     : Pat '->' cmds        { Alt $1 $3 }

Pat     : empty                { Empty }
        | lamda                { Lambda }
        | debris               { Debris }
        | asteroid             { Asteroid }
        | boundary             { Boundary }
        | '_'                  { Rest }

Dir     : right                { Parser.Right }
        | left                 { Parser.Left }
        | front                { Front }

{
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

newtype Program = Program { rules :: [Rule] }
    deriving Show

data Rule = Rule { id :: Ident,
                   cmds :: [Cmd] }
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

data Alt = Alt { pat     :: Pat,
                 altCmds :: [Cmd] }
  deriving Show

data Pat = Lambda
         | Debris
         | Asteroid
         | Boundary
         | Empty
         | Rest
  deriving Show

data Dir = Right
         | Left
         | Front
  deriving Show
}
