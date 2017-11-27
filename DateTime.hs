import Prelude hiding ((<*))
import ParseLib.Abstract

-- Kevin Wilbrink & Jordi Wippert

-- Starting Framework

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year { unYear :: Int }  deriving (Eq, Ord)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day { unDay :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour { unHour :: Int } deriving (Eq, Ord)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord)


-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = interact (printOutput . processCheck . processInput)
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show

parse2D :: Parser Char Int
parse2D = (\a b -> (mergeInts (a:[b]))) <$> newdigit <*> newdigit

parse4D :: Parser Char Int
parse4D = (\a b c d -> (mergeInts (a:b:c:[d]))) <$> newdigit <*> newdigit <*> newdigit <*> newdigit

parseTime :: Parser Char Time
parseTime = (\a b c -> Time (Hour a) (Minute b) (Second c)) <$> parse2D <*> parse2D <*> parse2D

parseDate :: Parser Char Date
parseDate = (\a b c -> Date (Year a) (Month b) (Day c)) <$> parse4D <*> parse2D <*> parse2D <* symbol 'T'

parseUtc :: Parser Char Bool
parseUtc = parseBool <$> option (symbol 'Z') ' '

parseBool :: Char -> Bool
parseBool 'Z' = True
parseBool _   = False

mergeInts :: [Int] -> Int
mergeInts []       = 0
mergeInts l@(x:xs) = 10^i * x + mergeInts xs
  where i = length l - 1

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseUtc

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run _ [] = Nothing
run p cs | null (parse p cs) = Nothing
         | otherwise         = Just (fst (head (parse p cs)))

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime (Date (Year x) (Month y) (Day z)) (Time (Hour a) (Minute b) (Second c)) i) =
  concat [show x, f' y, f' z, "T", f' a, f' b, f' c, boolChar i]
    where f' f | f < 10    = "0" ++ show f
               | otherwise = show f

boolChar :: Bool -> String
boolChar True = "Z"
boolChar _    = ""

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime (Date y mon d) (Time h (Minute min) (Second s)) _) =
  validYear y && validMonth mon && validDay y mon d && validHour h && validMS min && validMS s

validYear :: Year -> Bool
validYear (Year y) | y > 0     = True
                   | otherwise = False

validMonth :: Month -> Bool
validMonth (Month m) | m >= 1 && m <= 12 = True
                     | otherwise         = False

isLeapYear :: Year -> Bool
isLeapYear (Year y) = (mod y 4 == 0 && mod y 100 /= 0) || (mod y 400 == 0)

validDay :: Year -> Month -> Day -> Bool
validDay y (Month m) (Day d)
  | m == 2 && isLeapYear y && d > 29       = False
  | m == 2 && not (isLeapYear y) && d > 28 = False
  | m `mod` 2 == 0 && d > 30               = False
  | m `mod` 2 == 1 && d > 31               = False
  | m == 9 && d > 30                       = False
  | m == 10 && d > 31                      = False
  | m == 11 && d > 30                      = False
  | m == 12 && d > 31                      = False
  | otherwise                              = True

validHour :: Hour -> Bool
validHour (Hour h) | h >= 0 && h <= 23 = True
                   | otherwise         = False

validMS :: Int -> Bool
validMS m | m >= 0 && m <= 59 = True
          | otherwise         = False

-- Exercise 6
data Calendar = Calendar {
  calProp :: [CalProp],
  event   :: [Event] }

data CalProp = CalProp {
  prodId  :: String,
  version :: Float}

newtype Event = Event { eventProp :: [EventProp] }

data EventProp = EventProp {
  dtStamp     :: DateTime,
  uid         :: String,
  dtStart     :: DateTime,
  dtEnd       :: DateTime,
  description :: Maybe String,
  summary     :: Maybe String,
  location    :: String }
