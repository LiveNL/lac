-- Jordi Wippert & Kevin Wilbrink
{
module Parser where
import Data.List
import Scan
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
ident           { TIdent $$ }

%%

{- Exercise 3 -}
Program : rules                { reverse $1 }

rules   : Rule                 { [$1] }
        | rules Rule           { $2 : $1 }

Rule    : ident '->' cmds '.'  { Rule $1 (reverse $3) }

cmds    : Cmd                  { [$1] }
        | cmds ',' Cmd         { $3 : $1 }

Cmd     : go                   { Go }
        | take                 { Take }
        | mark                 { Mark }
        | nothing              { Parser.Nothing }
        | turn Dir             { Turn $2 }
        | case Dir of alts end { Case $2 $4 }
        | ident                { Next $1 }

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

{- Exercise 2 -}
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
  deriving (Show, Eq)

data Alt = Alt Contents [Cmd]
  deriving (Show, Eq)

data Contents = Lambda
              | Debris
              | Asteroid
              | Boundary
              | Empty
              | Rest
  deriving (Show, Eq)

data Dir = Right
         | Left
         | Front
  deriving (Show, Eq)

{- Exercise 5 -}
type ProgramAlgebra p r x a = ([r] -> p,            -- program
                              Ident -> [x] -> r,    -- rule
                              x,                    -- go
                              x,                    -- take
                              x,                    -- mark
                              x,                    -- nothing
                              Dir -> x,             -- turn
                              Dir -> [a] -> x,      -- case
                              Ident -> x,           -- next
                              Contents -> [x] -> a) -- alt

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

{- Exercise 6 -}
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
allContents :: ProgramAlgebra Bool Bool Bool Contents
allContents = ((\xs -> or xs), (\_ xs -> or xs), False, False, False, False, (\x -> False), (\_ xs -> r xs), (\x -> False), (\x _ -> x))
  where r xs = elem Rest xs || (length (nub xs) == 5) && (all def xs)
        def x = elem x [Lambda, Debris, Asteroid, Boundary, Empty]

-- check (Program (parseProgram [Token]))
check :: Program -> Bool
check p = and [u,t,s,a]
  where u = foldCmd notUndef p
        t = foldCmd notTwice p
        s = foldCmd hasStart p
        a = foldCmd allContents p
}
