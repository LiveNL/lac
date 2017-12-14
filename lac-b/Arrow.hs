-- Jordi Wippert & Kevin Wilbrink

module Arrow where

import Prelude hiding ((<*), (<$), Right, Left, Nothing)
import ParseLib.Abstract
import Data.Map (Map)
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)
import qualified Data.Maybe as M

import Debug.Trace


type Space     =  Map Pos Contents
type Size      =  Int
type Pos       =  (Int, Int)
data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary

parseSpace :: Parser Char Space
parseSpace =
  do
    (mr,mc)  <-  parenthesised
                   ((,) <$> natural <* symbol ',' <*> natural) <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <-  replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
             zipWith (\ r cs  ->
             zipWith (\ c d   ->  ((r,c),d)) [0..] cs) [0..] css

spaces :: Parser Char String
spaces = greedy (satisfy isSpace)

contents :: Parser Char Contents
contents =
  choice (Prelude.map (\ (f,c) -> f <$ symbol c) contentsTable) <* spaces

contentsTable :: [(Contents,Char)]
contentsTable =
  [  (Empty,'.'),(Lambda,'\\'),(Debris,'%'),(Asteroid,'O'),(Boundary,'#')]

-- These three should be defined by you
type Ident = ()
type Commands = ()
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack
  deriving Show

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String
  deriving Show

newtype Program = Program { rules :: [Rule] }
    deriving Show

data Rule = Rule Ident [Cmd]
    deriving Show

data Cmd = Go
         | Take
         | Mark
         | Nothing
         | Turn Dir
         | Case Dir [Alt]
         | Next String
    deriving (Eq, Show)

data Alt = Alt Contents [Cmd]
    deriving (Eq, Show)

data Dir = Right
         | Left
         | Front
    deriving (Eq, Show)

{- Exercise 9 -}
-- Test data
spc = fst ( head ( parse parseSpace "(4,14)\n\\\\\\\\\\..........\n...............\n\\\\\\\\\\\\\\........\n...............\n...............\n"))
env = L.fromList [("start",[Case Left [Alt Asteroid [Next "goOn"],Alt Boundary [Next "goOn"],Alt Lambda [Turn Left,Go,Take],Alt Rest [Turn Left,Go,Next "start"]]])]
env1 = L.fromList [("start", [Turn Right,Go,Turn Left,Next "firstArg"])]
arS = ArrowState spc (0,1) Right [Turn Right,Go,Turn Left,Next "firstArg"]

step :: Environment -> ArrowState -> Step
step e a@(ArrowState _ _ _ st) = action topItem a e
  where topItem = head (st)

action :: Cmd -> ArrowState -> Environment -> Step
action Go (ArrowState sp p h (_:xs)) _ | field' == Empty || field' == Lambda || field' == Debris = Ok (ArrowState sp pos' h xs)
                                       | otherwise                                               = Ok (ArrowState sp p    h xs) 
  where field' = nextField h p sp
        pos'   = nextPos h p

action Take (ArrowState sp p h (_:xs)) _ = Ok (ArrowState sp' p h xs)
  where sp' = L.insert p Empty sp

action Mark (ArrowState sp p h (_:xs)) _ = Ok (ArrowState sp' p h xs)
  where sp' = L.insert p Lambda sp

action (Nothing) e _ = Ok e

action (Turn x)   (ArrowState sp p _ (_:xs)) _ = Ok (ArrowState sp p x xs)

action (Case x c) (ArrowState sp p h (_:xs)) _ | null test = Fail "Error"
                                               | otherwise = Ok (ArrowState sp p h (test ++ xs))
  where field' = nextField x p sp
        test = getCmd field' c

action (Next x) (ArrowState sp p h (_:xs)) e | find /= M.Nothing = Ok (ArrowState sp p h ((M.fromJust find) ++ xs))
                                             | otherwise         = Fail "Stack is empty.."
  where find = lookup x (L.toList e)

getCmd :: Contents -> [Alt] -> [Cmd]
getCmd f [] = []
getCmd f ((Alt co cmd):xs) | co == f    = cmd
                           | co == Rest = cmd
                           | otherwise  = getCmd f xs

nextField :: Heading -> Pos -> Space -> Contents
nextField Right (x,y) s = M.fromJust (lookup (x, y + 1) (L.toList s))
nextField Left  (x,y) s = M.fromJust (lookup (x, y - 1) (L.toList s))
nextField _     _     _ = Boundary

nextPos :: Heading -> Pos -> Pos
nextPos Right (x, y) = (x, y + 1)
nextPos Left  (x, y) = (x, 1 - y)
