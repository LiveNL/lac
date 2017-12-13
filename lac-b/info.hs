module Info where 

import Data.Map (Map)

type Space     =  Map Pos Contents
type Size      =  Int
type Pos       =  (Int, Int)
data Contents  =  Empty | Lambda | Debris | Asteroid | Boundary
  deriving (Eq, Show)

newtype Program = Program { rules :: [Rule] }
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
         | Ident String
    deriving Show

data Alt = Alt Contents [Cmd]
    deriving Show

data Dir = Right
         | Left
         | Front
    deriving Show