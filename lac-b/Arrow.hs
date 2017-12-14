-- Jordi Wippert & Kevin Wilbrink

module Arrow where

import Prelude hiding ((<*), (<$))
import ParseLib.Abstract
import Data.Map (Map)
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.Maybe hiding (Nothing)

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
type Heading = Dir

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step  =  Done  Space Pos Heading
           |  Ok    ArrowState
           |  Fail  String

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
contentToString c = [(fromJust (lookup c contentsTable))]

{- Exercise 8 -}
toEnvironment :: String -> Environment -- Check nog implementeren(!!)
toEnvironment s = L.fromList [(i, c) | (Rule i c) <- program]
  where scanParse = parseProgram (alexScanTokens s)
        program = if check (Program scanParse)
                  then scanParse
                  else error "Failed to add parse program (check failed)"

{- Exercise 9 -}
step :: Environment -> ArrowState -> Step
step = undefined

{- Exercise 10
Note how recursion affects the size of the command stack during execution. Does it matter whether the recursive call is in the middle of a command sequence or at the very end of the command sequence? Include your observations as a comment.

Recursion affects the size in such way that when a call is done to a 'Next' (our data type for identifying Identifiers as commands), it will add the commands of the corresponding rule to the stack, this could increase the stack unlimited.

The added commands will be executed before the remaining stack. I.E. if a recursing call is being done (so, to itself), in the middle of the commands from the rule that it was executing, the rest of these commands will be only after it executed its added list of commands. If the recursive call is in the end of the list, all commands will be executed already before adding the new commands to the list. So it only matters to the order of execution instead of affecting the size when it would be executed from the middle.

-}


