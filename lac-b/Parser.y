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
  deriving Show

data Dir = Right
         | Left
         | Front
  deriving Show

-- main = getContents >>= print . parseProgram
main = undefined

type ProgramAlgebra p r x a = ([r] -> p,         -- program
                              Ident -> [x] -> r, -- rule
                              x,                 -- go
                              x,                 -- mark
                              x,                 -- nothing
                              Dir -> x,          -- turn
                              Dir -> [a] -> x,   -- case
                              Ident -> x,        -- next
                              Pat -> [x] -> a)   -- alt

foldCmd :: ProgramAlgebra p r x a -> Program -> p
foldCmd (program, rule, go, mark, nothing, turn, c, next, alt) = fp
  where fp (Program xs) = program (map fr xs)
        fr (Rule x xs) = rule x (map ff xs)
        ff Go = go
        ff Mark = mark
        ff Parser.Nothing = nothing
        ff (Turn x) = turn x
        ff (Case x xs) = c x (map fa xs)
        ff (Next x) = next x
        fa (Alt x xs) = alt x (map ff xs)

{-
foldProgram :: ProgramAlgebra p -> Program -> p
foldProgram (program, rule, cmd, alt, pat, dir, id) = ff
   where ff (Program xs) = program (map fr xs)
         fr (Rule x xs)  = rule (fi x) (map fc xs)
         fc x            = cmd x
         fc (Turn x)     = cmd x
         fa (Alt x xs)   = alt (fp x) (map fc xs)
         fp x            = pat x
         fd x            = dir x
         fi x            = id x
-}

{-
type ProgramAlgebra x = ([x] -> x,          -- Program
                         x -> [x] -> x,     -- Rule
                         Cmd -> x,          -- Cmd
                         x -> [x] -> x,     -- Alt
                         Pat -> x,          -- Pat
                         Dir -> x)          -- Dir

foldProgram :: ProgramAlgebra p -> Program -> p
foldProgram (program, rule, cmd, alt, pat, dir) = ff
   where ff (Program xs) = program (map fr xs)
         fr (Rule x xs)  = rule x (map fc xs)
         fc x            = cmd x
         fa (Alt x xs)   = alt x (map fc xs)
         fp x            = pat x
         fd x            = dir x
-}

{-
type ProgramAlgebra x = ([x] -> x,          -- Program
                        x -> [x] -> x,     -- Rule
                        Dir -> x,          -- Cmd1 (Turn Dir)
                        Dir -> [x] -> x,   -- Cmd2 (Case Dir [Alt])
                        Pat -> [x] -> x,   -- Alt
                        Dir -> x          )-- Dir

foldProgram :: ProgramAlgebra p -> Program -> p
foldProgram (program, rule, cmd1, cmd2, alt, dir) = ff
   where ff (Program xs) = program (map fr xs)
         fr (Rule x xs)  = rule x (map fc xs)
         fc (Turn x)     = cmd1 x
         fc (Case x xs)  = cmd2 x (map fa xs)
         fa (Alt x xs)   = alt x (map fc xs)

hasStart :: ProgramAlgebra Bool
hasStart = ((\x -> False), (\x -> r), (\x -> False), (\x -> False), False, False)
  where r x = x == "start"

check :: Program -> Bool
check = foldProgram hasStart
-}

}
