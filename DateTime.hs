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

parseSecond :: Parser Char Second
parseSecond = (\a b -> Second (test ([a]++[b]))) <$> newdigit <*> newdigit

parseMinute :: Parser Char Minute
parseMinute = (\a b -> Minute (test ([a]++[b]))) <$> newdigit <*> newdigit

parseHour :: Parser Char Hour
parseHour = (\a b -> Hour (test ([a]++[b]))) <$> newdigit <*> newdigit

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseYear :: Parser Char Year
parseYear = (\a b c d -> Year (test ([a]++[b]++[c]++[d]))) <$> newdigit <*> newdigit <*> newdigit <*> newdigit

parseMonth :: Parser Char Month
parseMonth = (\a b -> Month (test ([a]++[b]))) <$> newdigit <*> newdigit

parseDay :: Parser Char Day
parseDay = (\a b _ -> Day (test ([a]++[b]))) <$> newdigit <*> newdigit <* symbol 'T'

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parse1 :: Parser Char Bool
parse1 = (\x -> parseBool x) <$> option (symbol 'Z') 'E'

parseBool :: Char -> Bool
parseBool 'Z' = True
parseBool _   = False

test :: [Int] -> Int
test []       = 0
test l@(x:xs) = 10^i * x + test xs
  where i = (length l) - 1

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parse1

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run _ [] = Nothing
run p cs = if null (parse p cs)
           then Nothing
           else Just (fst (head (parse p cs)))



dt = DateTime { date = Date { year = Year 2016, month = Month 2, day = Day 29 },
                time = Time { hour = Hour 03, minute = Minute 49, second = Second 18 },
                utc = True }

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime (Date (Year x) (Month y) (Day z)) (Time (Hour a) (Minute b) (Second c)) i) =
  show x ++ check y ++ check z ++ "T" ++ check a ++ check b ++ check c ++ boolChar i

boolChar :: Bool -> [Char]
boolChar True = "Z"
boolChar _    = ""

check :: Int -> String
check i | i < 10     = "0" ++ show i
        | otherwise = show i

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime (Date y mon d) (Time h min s) i) = 
  validYear y && validMonth mon && validDay y mon d && validHour h && validMinute min && validSecond s

validYear :: Year -> Bool
validYear (Year y) | y >= 1000 = True -- Iets met BC doen?
                   | otherwise = False

validMonth :: Month -> Bool
validMonth (Month m) | m >= 1 && m <= 12 = True
                     | otherwise         = False

isLeapYear :: Year -> Bool
isLeapYear (Year y) = if (mod y 4 == 0 && mod y 100 /= 0) || (mod y 400 == 0) then True else False

validDay :: Year -> Month -> Day -> Bool
validDay (Year y) (Month m) (Day d) | m == 1 && (d >= 1 && d <= 30)                                = True
                                    | m == 2 && (d >= 1 && d <= 28 && isLeapYear (Year y) /= True) = True
                                    | m == 2 && (d >= 1 && d <= 29 && isLeapYear (Year y))         = True
                                    | m == 3 && (d >= 1 && d <= 31)                                = True
                                    | m == 4 && (d >= 1 && d <= 30)                                = True
                                    | m == 5 && (d >= 1 && d <= 31)                                = True
                                    | m == 6 && (d >= 1 && d <= 30)                                = True
                                    | m == 7 && (d >= 1 && d <= 31)                                = True
                                    | m == 8 && (d >= 1 && d <= 31)                                = True
                                    | m == 9 && (d >= 1 && d <= 30)                                = True
                                    | m == 10 && (d >= 1 && d <= 31)                               = True
                                    | m == 11 && (d >= 1 && d <= 30)                               = True
                                    | m == 12 && (d >= 1 && d <= 31)                               = True
                                    | otherwise                                                    = False


validHour :: Hour -> Bool
validHour (Hour h) | h >= 0 && h <= 23 = True
                   | otherwise         = False

validMinute :: Minute -> Bool
validMinute (Minute m) | m >= 0 && m <= 59 = True
                       | otherwise         = False

validSecond :: Second -> Bool
validSecond (Second s) | s >= 0 && s <= 59 = True
                       | otherwise         = False
-- Exercise 6

