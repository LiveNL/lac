module ICalendar where

import Prelude hiding ((<*), (*>), (<$), ($>))
import ParseLib.Abstract
import Data.Maybe


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


data Calendar = Calendar { prodId :: String
                         , events :: [VEvent] }
    deriving (Eq, Show)

data VEvent = VEvent { dtStamp     :: DateTime
                     , uid         :: String
                     , dtStart     :: DateTime
                     , dtEnd       :: DateTime
                     , description :: Maybe String
                     , summary     :: Maybe String
                     , location    :: Maybe String }
    deriving (Eq, Show)

-- DateTime parseing, copied from exercise 1
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

parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseUtc
-- End -- DateTime parseing, copied from exercise 1

-- "Main" block, DO NOT EDIT.
-- If you want to run the parser + pretty-printing, rename this module (first line) to "Main".
-- DO NOT forget to rename the module back to "ICalendar" before submitting to DomJudge.
run :: Parser a b -> [a] -> Maybe b
run p s = listToMaybe [p | (p, []) <- parse p s]

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar


main = do
    res <- readCalendar "examples/rooster_infotc.ics"
    putStrLn $ maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11)) res

-- Exercise 1
data Token = TProdid      String
           | TVersion     String
           | TBegin       String
           | TSummary     String
           | TDescription String
           | TLocation    String
           | TUID         String
           | TDTStamp     String
           | TDTStart     String
           | TDTEnd       String
           | TEnd         String
           | Rest
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = many (toToken <$> identifier <* symbol ':' <*> greedy (satisfy (\x -> x /= '\r')) <* symbol '\r' <* symbol '\n')

toToken :: String -> String -> Token
toToken a b = case a of
  "BEGIN"       -> TBegin       b
  "PRODID"      -> TProdid      b
  "VERSION"     -> TVersion     b
  "SUMMARY"     -> TSummary     b
  "DESCRIPTION" -> TDescription b
  "LOCATION"    -> TLocation    b
  "UID"         -> TUID         b
  "DTSTAMP"     -> TDTStamp     b
  "DTSTART"     -> TDTStart     b
  "DTEND"       -> TDTEnd       b
  "END"         -> TEnd         b
  _             -> Rest

parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$ (satisfy isBegin) <*> parseProdID <* (satisfy isBegin) <*> many toEvent <* (satisfy isEnd)

parseProdID :: Parser Token String
parseProdID = toString <$> satisfy isProdId

isProdId :: Token -> Bool
isProdId (TProdid _) = True
isProdId _           = False

toEvent :: Parser Token VEvent
toEvent = VEvent <$ (satisfy isBegin) <*> parseDT <*> parseUID <*> parseDT <*> parseDT <*> optional parseMaybe <*> optional parseMaybe <*> optional parseMaybe <* (satisfy isEnd)

parseUID :: Parser Token String
parseUID = toString <$> satisfy isUID

isUID :: Token -> Bool
isUID (TUID _) = True
isUID _        = False

parseDT :: Parser Token DateTime
parseDT = toDT <$> satisfy isDt

toDT :: Token -> DateTime
toDT (TDTStamp x) = fst (head (parse parseDateTime x))
toDT (TDTStart x) = fst (head (parse parseDateTime x))
toDT (TDTEnd x)   = fst (head (parse parseDateTime x))
toDT _            = error "TimeStamp"

isDt :: Token -> Bool
isDt (TDTStamp _) = True
isDt (TDTStart _) = True
isDt (TDTEnd _)   = True
isDt _            = False

isBegin :: Token -> Bool
isBegin (TBegin _)   = True
isBegin (TVersion _) = True
isBegin _            = False

isEvent :: Token -> Bool
isEvent (TEnd _) = False
isEvent _        = True

isEnd :: Token -> Bool
isEnd (TEnd _) = True
_              = False

parseMaybe :: Parser Token String
parseMaybe = toString <$> satisfy isMaybe

toString :: Token -> String
toString (TSummary x)     = x
toString (TDescription x) = x
toString (TLocation x)    = x
toString (TProdid x)      = x
toString (TUID x)         = x

isMaybe :: Token -> Bool
isMaybe (TSummary _)     = True
isMaybe (TDescription _) = True
isMaybe (TLocation _)    = True
isMaybe _                = False

-- Exercise 2
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar = undefined


-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = undefined


-- Exercise 4
countEvents :: Calendar -> Int
countEvents = undefined

findEvents :: DateTime -> Calendar -> [VEvent]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined



-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined

