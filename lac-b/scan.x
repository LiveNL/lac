{
module Scan where
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-
  $white+     ;
  "--".*      ;
  "->"        {\s -> TArrow}
  "."         {\s -> TDot}
  ","         {\s -> TComma}
  "go"        {\s -> TGo}
  "take"      {\s -> TTake}
  "mark"      {\s -> TMark}
  "nothing"   {\s -> TNothing}
  "turn"      {\s -> TTurn}
  "case"      {\s -> TCase}
  "of"        {\s -> TOf}
  "end"       {\s -> TEnd}
  "left"      {\s -> TLeft}
  "right"     {\s -> TRight}
  "front"     {\s -> TFront}
  ";"         {\s -> TSemicolon}
  "Empty"     {\s -> TEmpty}
  "Lambda"    {\s -> TLambda}
  "Debris"    {\s -> TDebris}
  "Asteroid"  {\s -> TAsteroid}
  "Boundary"  {\s -> TBoundary}
  "_"         {\s -> TUnderscore}
  $alpha+     {\s -> TLetter s}
  $digit+     {\s -> TDigit (read s) }
  "+"+        {\s -> TPlus}
  "-"+        {\s -> TMinus}


{
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

main = do s <- getContents
          print (alexScanTokens s)

}
