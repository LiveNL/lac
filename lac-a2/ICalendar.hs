module ICalendar where

-- Kevin Wilbrink & Jordi Wippert
--
import Prelude hiding ((<*), (*>), (<$), ($>))
import ParseLib.Abstract
import Data.Maybe
import System.IO
import Debug.Trace
import Data.List


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
    deriving Eq

data VEvent = VEvent { dtStamp     :: DateTime
                     , uid         :: String
                     , dtStart     :: DateTime
                     , dtEnd       :: DateTime
                     , description :: Maybe String
                     , summary     :: Maybe String
                     , location    :: Maybe String }
    deriving Eq

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

printDateTime :: DateTime -> String
printDateTime (DateTime (Date (Year x) (Month y) (Day z)) (Time (Hour a) (Minute b) (Second c)) i) =
  concat [show x, f' y, f' z, "T", f' a, f' b, f' c, boolChar i]
    where f' f | f < 10    = "0" ++ show f
               | otherwise = show f

boolChar :: Bool -> String
boolChar True = "Z"
boolChar _    = ""
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

isSpace :: Char -> Bool
isSpace ' ' = True
isSpace _   = False

spaces :: Parser Char String
spaces =  greedy (satisfy isSpace)

scanCalendar :: Parser Char [Token]
scanCalendar = many (toToken <$ spaces <*> identifier <* option (symbol ':') ':' <*> greedy (satisfy (\x -> x /= '\r')) <* symbol '\r' <* symbol '\n')

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

-- testCalendar fst (head (parse scanCalendar "BEGIN:VCALENDAR\r\nPRODID:-//hacksw/handcal//NONSGML v1.0//EN\r\nVERSION:2.0\r\nBEGIN:VEVENT\r\nSUMMARY:Bastille Day Party\r\nUID:19970610T172345Z-AF23B2@example.com\r\nDTSTAMP:19970610T172345Z\r\nDTSTART:19970714T170000Z\r\nDTEND:19970715T040000Z\r\nEND:VEVENT\r\nEND:VCALENDAR\r\n"))

parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$ (satisfy isBegin) <*> parseProdID <* option parseRest "" <* (satisfy isBegin) <*> many toEvent <* (satisfy isEnd)

parseProdID :: Parser Token String
parseProdID = toString <$> satisfy isProdId

isProdId :: Token -> Bool
isProdId (TProdid _) = True
isProdId _           = False

toEvent :: Parser Token VEvent
toEvent = VEvent <$ (satisfy isBegin) <*> parseDT <*> parseUID <* option parseRest "" <*> parseDT <*> parseDT <*> optional parseDesc <* option parseRest "" <*> optional parseSummary <* option parseRest "" <*> optional parseLocation <* option parseRest "" <* (satisfy isEnd)

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
isEnd _        = False

parseDesc :: Parser Token String
parseDesc = toString <$> satisfy isDesc

isDesc :: Token -> Bool
isDesc (TDescription _) = True
isDesc _ = False

parseSummary :: Parser Token String
parseSummary = toString <$> satisfy isSummary

isSummary :: Token -> Bool
isSummary (TSummary _) = True
isSummary _ = False

parseLocation :: Parser Token String
parseLocation = toString <$> satisfy isLocation

isLocation :: Token -> Bool
isLocation (TLocation _) = True
isLocation _ = False

parseRest :: Parser Token String
parseRest = toString <$> satisfy isRest

isRest :: Token -> Bool
isRest (Rest) = True
isRest _ = False

toString :: Token -> String
toString (TSummary x)     = x
toString (TDescription x) = x
toString (TLocation x)    = x
toString (TProdid x)      = x
toString (TUID x)         = x
toString (Rest)           = ""

-- Exercise 2
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar p = do f <- openFile p ReadMode
                    d <- hGetContents f
                    return (recognizeCalendar d)

readCalendar' :: FilePath -> IO String
readCalendar' p = do f <- openFile p ReadMode
                     d <- hGetContents f
                     return d

-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar (Calendar p e) =
  "BEGIN:VCALENDAR\r\n"     ++
  "PRODID:" ++ p ++ "\r\n"  ++
  "VERSION:2.0" ++ "\r\n"  ++
   concat (map printEvent e) ++
  "END:VCALENDAR\r\n"

printEvent :: VEvent -> String
printEvent (VEvent dtStamp uid dtStart dtEnd des sum loc) =
  "BEGIN:VEVENT\r\n"                            ++
  "UID:"     ++ uid                   ++ "\r\n" ++
  "DTSTAMP:" ++ printDateTime dtStamp ++ "\r\n" ++
  "DTSTART:" ++ printDateTime dtStart ++ "\r\n" ++
  "DTEND:"   ++ printDateTime dtEnd   ++ "\r\n" ++
  printMaybe "SUMMARY:"     sum                 ++
  printMaybe "DESCRIPTION:" des                 ++
  printMaybe "LOCATION:"    loc                 ++
  "END:VEVENT\r\n"

printMaybe :: String -> Maybe String -> String
printMaybe _ Nothing  = ""
printMaybe x (Just y) = x ++ y ++ "\r\n"

-- Exercise 4
countEvents :: Calendar -> Int
countEvents (Calendar _ e) = length e

findEvents :: DateTime -> Calendar -> [VEvent]
findEvents dt (Calendar _ e) = [ev | ev@(VEvent _ _ start end _ _ _) <- e, dt >= start && dt <= end]

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ e) | length e <= 2 = check2 [e]
                                | otherwise =  check2 (f' 2 e)

check2 :: [[VEvent]] -> Bool
check2 []     = False
check2 (x:xs) = if o h t then True else check2 xs
  where h = head x
        t = last x
        o (VEvent _ _ start1 end1 _ _ _) (VEvent _ _ start2 end2 _ _ _) | (start1 < end2 && start2 < end1) || (end1 == start2) || (start1 == end2) = True
                                                                        | otherwise = False

f' :: (Eq t, Num t) => t -> [a] -> [[a]]
f' 0 _  = [[]]
f' _ [] = []
f' k as = [ x : xs | (x:as') <- tails as, xs <- f' (k-1) as' ]

timeSpent :: String -> Calendar -> Int
timeSpent f (Calendar p e) = undefined --  [ev | ev@(VEvent _ _ start end _ sum _) <- e, f == sum]

-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined

