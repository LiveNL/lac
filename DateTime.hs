import ParseLib.Abstract

import Debug.Trace
-- Starting Framework


-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
    deriving (Eq, Ord, Show)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord, Show)

newtype Year  = Year { unYear :: Int }  deriving (Eq, Ord, Show)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord, Show)
newtype Day   = Day { unDay :: Int } deriving (Eq, Ord, Show)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord, Show)

newtype Hour   = Hour { unHour :: Int } deriving (Eq, Ord, Show)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord, Show)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord, Show)


-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

--instance Show DateTime where
 --   show = printDateTime

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

parseSecond :: Parser Char Second
parseSecond = (\a b -> Second (mergeInts (a:[b]))) <$> newdigit <*> newdigit

parseMinute :: Parser Char Minute
parseMinute = (\a b -> Minute (mergeInts (a:[b]))) <$> newdigit <*> newdigit

parseHour :: Parser Char Hour
parseHour = (\a b -> Hour (mergeInts (a:[b]))) <$> newdigit <*> newdigit

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseYear :: Parser Char Year
parseYear = (\a b c d -> Year (mergeInts (a:b:c:[d]))) <$> newdigit <*> newdigit <*> newdigit <*> newdigit

parseMonth :: Parser Char Month
parseMonth = (\a b -> Month (mergeInts (a:[b]))) <$> newdigit <*> newdigit

parseDay :: Parser Char Day
parseDay = (\a b _ -> Day (mergeInts  (a:[b]))) <$> newdigit <*> newdigit <*> symbol 'T'

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseUtc :: Parser Char Bool
parseUtc = parseBool <$> identifier

parseBool :: String -> Bool
parseBool "Z" = True
parseBool _   = False

mergeInts :: [Int] -> Int
mergeInts []       = 0
mergeInts l@(x:xs) = 10^i * x + mergeInts xs
  where i = (length l) - 1

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseUtc

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run _ [] = Nothing
run p cs = Just (fst (head (parse p cs)))

dt = DateTime { date = Date { year = Year 2017, month = Month 11, day = Day 20 },
                time = Time { hour = Hour 03, minute = Minute 09, second = Second 18 },
                utc = True }

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime (Date (Year x) (Month y) (Day z)) (Time (Hour a) (Minute b) (Second c)) i) =
  concat (show x : (f' y) : (f' z) : "T" : (f' a) : (f' b) : (f' c) : (g' i) : [])
    where f' f = if f < 10
                 then "0" ++ (show f)
                 else show f
          g' g = if g
                 then "Z"
                 else ""

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
-- Exercise 6

