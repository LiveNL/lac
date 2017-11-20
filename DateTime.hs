import ParseLib.Abstract


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
parseSecond = Second <$> integer

parseMinute :: Parser Char Minute
parseMinute = Minute <$> integer

parseHour :: Parser Char Hour
parseHour = Hour <$> integer

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseYear :: Parser Char Year
parseYear = Year <$> integer

parseMonth :: Parser Char Month
parseMonth = Month <$> integer

parseDay :: Parser Char Day
parseDay = Day <$> integer

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseUtc :: Parser Char Bool
parseUtc = parseBool <$> digit

parseBool :: Char -> Bool
parseBool 'Z' = True
parseBool _ = False

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseUtc

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run _ [] = Nothing
run p cs = Just (fst (head (parse p cs)))


-- Exercise 3
printDateTime :: DateTime -> String
printDateTime = undefined



-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s




-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined



-- Exercise 6

