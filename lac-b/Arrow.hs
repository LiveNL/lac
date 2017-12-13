-- Jordi Wippert & Kevin Wilbrink

module Arrow where

import Prelude hiding ((<*), (<$))
import ParseLib.Abstract
import Data.Map (Map)
import qualified Data.Map as L
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.Maybe

import Debug.Trace


type Space     =  Map Pos Contents
type Size      =  Int
type Pos       =  (Int, Int)
data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary
  deriving (Eq, Show)

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
type Ident = String
type Commands = [Cmd]
type Heading = ()

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
field=fst(head([(L.fromList [((0,0),Empty),((0,1),Empty),((0,2),Empty),((0,3),Empty),((0,4),Empty),((0,5),Empty),((0,6),Empty),((0,7),Empty),((1,0),Empty),((1,1),Empty),((1,2),Empty),((1,3),Empty),((1,4),Debris),((1,5),Empty),((1,6),Empty),((1,7),Empty),((2,0),Empty),((2,1),Empty),((2,2),Debris),((2,3),Debris),((2,4),Debris),((2,5),Debris),((2,6),Empty),((2,7),Empty),((3,0),Empty),((3,1),Empty),((3,2),Empty),((3,3),Empty),((3,4),Debris),((3,5),Debris),((3,6),Debris),((3,7),Empty),((4,0),Empty),((4,1),Empty),((4,2),Empty),((4,3),Debris),((4,4),Debris),((4,5),Debris),((4,6),Empty),((4,7),Empty),((5,0),Empty),((5,1),Empty),((5,2),Empty),((5,3),Empty),((5,4),Debris),((5,5),Empty),((5,6),Debris),((5,7),Debris),((6,0),Empty),((6,1),Empty),((6,2),Empty),((6,3),Empty),((6,4),Debris),((6,5),Debris),((6,6),Debris),((6,7),Debris),((7,0),Empty),((7,1),Empty),((7,2),Empty),((7,3),Empty),((7,4),Empty),((7,5),Empty),((7,6),Empty),((7,7),Empty)],"")]))

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
toEnvironment :: String -> Environment
toEnvironment = undefined

{- Exercise 9 -}
step :: Environment -> ArrowState -> Step
step = undefined
