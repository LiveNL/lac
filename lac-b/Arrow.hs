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

import Scan
import Parser

type Space     =  Map Pos Contents
type Size      =  Int
type Pos       =  (Int, Int)
-- Moved Contents to the Parser, to the rest of the Program data-types
-- data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary
--   deriving (Eq, Show)

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
  [(Empty,'.'),(Lambda,'\\'),(Debris,'%'),(Asteroid,'O'),(Boundary,'#')]

-- These three should be defined by you
type Commands = [Cmd]
data Heading = North | East | South | West
  deriving (Show, Eq, Enum)

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack
  deriving Show

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String
  deriving Show

{- Exercise 4

Happy and right-recursion is not a good combination. Happy is more efficient at parsing using left-recursive rules.
Left-recursive results in a constant stack-space parser, wheres right-recursive rules require stack space proportional
to the length of the list being parsed. For example, the parser in GHC used to use right-recursion to parse lists,
and as a result it failed to parse some Happy-generated modules due to running out of stack space!

 -}

 {- Exercise 7 -}
spacePrinter :: (Show b, Show a, Eq a) => Map (a, b) Contents -> [Char]
spacePrinter s = pos' (last (L.toList s)) ++ printer (L.toList s)
  where pos' ((x, y), _) = "(" ++ show x ++ "," ++ show y ++ ")\n"

printer :: Eq a => [((a, b), Contents)] -> [Char]
printer [((x, y), z)] = contentToString z
printer (((x, y), z):k@((a, b), c):xs) | x == a    = contentToString z ++ printer (k: xs)
                                       | otherwise = contentToString z ++ "\n" ++ printer (k:xs)

contentToString :: Contents -> String
contentToString c = [(M.fromJust (lookup c contentsTable))]

{- Exercise 8 -}
toEnvironment :: String -> Environment
toEnvironment s = L.fromList [(i, c) | (Rule i c) <- program]
  where scanParse = parseProgram (alexScanTokens s)
        program = if check (Program scanParse)
                  then scanParse
                  else error "Failed to add parse program (check failed)"

{- Exercise 9 -}

next :: Heading -> Heading
next West = North
next d    = succ d

prev :: Heading -> Heading
prev North = West
prev d     = pred d

step :: Environment -> ArrowState -> Step
step e a@(ArrowState sp p h st) | null st   = Done sp p h
                                | otherwise = action (head st) a e


action :: Cmd -> ArrowState -> Environment -> Step
action Go (ArrowState sp p h (_:xs)) _ | field' == Empty || field' == Lambda || field' == Debris = Ok (ArrowState sp pos' h xs)
                                       | otherwise                                               = Ok (ArrowState sp p    h xs)
                                       where field' = nextField h p sp
                                             pos'   = nextPos h p

action Take (ArrowState sp p h (_:xs)) _ = Ok (ArrowState sp' p h xs)
  where sp' = L.insert p Empty sp

action Mark (ArrowState sp p h (_:xs)) _ = Ok (ArrowState sp' p h xs)
  where sp' = L.insert p Lambda sp

action Nothing (ArrowState sp p h (_:xs)) _ = Ok (ArrowState sp p h xs)

action (Turn x) (ArrowState sp p h (_:xs)) _ = Ok (ArrowState sp p newHeading xs)
  where newHeading | x == Front = h
                   | x == Right = next h
                   | x == Left = prev h

action (Case x c) (ArrowState sp p h (_:xs)) _ | null test = Fail "Error"
                                               | otherwise = Ok (ArrowState sp p h (test ++ xs))
  where field' = checkField x h p sp
        test = getCmd field' c

action (Next x) (ArrowState sp p h (_:xs)) e | find /= M.Nothing = Ok (ArrowState sp p h (((M.fromJust find)) ++ xs))
                                             | otherwise         = Fail "Stack is empty.."
  where find = (lookup x (reverse (L.toList e)))

getCmd :: Contents -> [Alt] -> [Cmd]
getCmd f [] = []
getCmd f ((Alt co cmd):xs) | co == f    = cmd
                           | co == Rest = cmd
                           | otherwise  = getCmd f xs

nextField :: Heading -> Pos -> Space -> Contents
nextField h (y,x) s = if M.isNothing n
                      then Boundary
                      else M.fromJust n
  where n = f h (y,x) s
        f East (y,x) s  = lookup (y,x + 1) (L.toList s)
        f South (y,x) s = lookup (y + 1,x) (L.toList s)
        f West (y,x) s  = lookup (y,x - 1) (L.toList s)
        f North (y,x) s = lookup (y - 1,x) (L.toList s)

checkField :: Dir -> Heading -> Pos -> Space -> Contents
checkField d h (x,y) s = if M.isNothing (n newHeading)
                         then Boundary
                         else M.fromJust (n newHeading)
  where n nh = f nh (x,y) s
        f East (y,x) s  = lookup (y,x + 1) (L.toList s)
        f South (y,x) s = lookup (y + 1,x) (L.toList s)
        f West (y,x) s  = lookup (y,x - 1) (L.toList s)
        f North (y,x) s = lookup (y - 1,x) (L.toList s)
        newHeading | d == Front = h
                   | d == Right = next h
                   | d == Left = prev h

nextPos :: Heading -> Pos -> Pos
nextPos East (y,x)  = (y,x + 1)
nextPos South (y,x) = (y + 1,x)
nextPos West (y,x)  = (y,x - 1)
nextPos North (y,x) = (y - 1,x)

{- Exercise 10
Note how recursion affects the size of the command stack during execution. Does it matter whether the recursive call is in the middle of a command sequence or at the very end of the command sequence? Include your observations as a comment.

Recursion affects the size in such way that when a call is done to a 'Next' (our data type for identifying Identifiers as commands), it will add the commands of the corresponding rule to the stack, this could increase the stack unlimited.

The added commands will be executed before the remaining stack. I.E. if a recursing call is being done (so, to itself), in the middle of the commands from the rule that it was executing, the rest of these commands will be only after it executed its added list of commands. If the recursive call is in the end of the list, all commands will be executed already before adding the new commands to the list. So it only matters to the order of execution instead of affecting the size when it would be executed from the middle.

-}

{- Exercise 11 -}

e = toEnvironment "start       -> turn right, go, turn left, firstArg.  turnAround  -> turn right, turn right.  return      -> case front of Boundary  ->  nothing; _         ->  go, return end.  firstArg    -> case left of Lambda  ->  go, firstArg, mark, go; _       ->  turnAround, return, turn left, go, go, turn left, secondArg end.  secondArg   -> case left of Lambda  ->  go, secondArg, mark, go; _       ->  turnAround, return, turn left, go, turn left end."

s = parse parseSpace "(4,14)\n\\\\\\\\\\..........\n...............\n\\\\\\\\\\\\\\........\n...............\n...............\n"

c = [Turn Right,Go,Turn Left,Next "firstArg"]
h = East
p = (0,0)
a = ArrowState (fst (head s)) p h c

interactive :: Environment -> ArrowState -> IO ()
interactive e a = do putStr (show (step e a) ++ "\n")
                     k <- getChar
                     case k of
                       'k' -> do interactive e (x (step e a))
  where x (Ok n) = n
        x (Fail n) = error "FUCK"
        x (Done _ _ _) = error "DONE"


