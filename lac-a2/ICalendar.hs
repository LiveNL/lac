module ICalendar where

-- Kevin Wilbrink & Jordi Wippert
--
import Prelude hiding ((<*), (*>), (<$), ($>), sequence)
import ParseLib.Abstract
import Data.Maybe
import System.IO
import Debug.Trace
import Data.List
import Text.PrettyPrint.Boxes


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

isLeapYear :: Year -> Bool
isLeapYear (Year y) = (mod y 4 == 0 && mod y 100 /= 0) || (mod y 400 == 0)

getDay :: Year -> Month -> Int
getDay (Year y) (Month m)
    | m == 2 && isLeapYear (Year y) = 29
    | m == 2 = 28
    | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
    | otherwise = 30

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
scanCalendar = many (toToken <$ spaces <*> identifier <* option (symbol ':') ':' <*> line)

line :: Parser Char [Char]
line = combineLine <$> many (greedy (satisfy (\x -> x /= '\r')) <* token "\r\n ") <*> greedy (satisfy (\x -> x /= '\r')) <* token "\r\n"

combineLine :: [String] -> String -> String
combineLine a b = concat a ++ b

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
parseCalendar = Calendar <$ parseBegin <*> ((parseVersion *> parseProdID) <|> (parseProdID <* parseVersion)) <*> many (pack parseBegin toEvent parseEnd) <* (satisfy isEnd)

parseVersion :: Parser Token String
parseVersion = toString <$> satisfy isVersion

parseBegin :: Parser Token String
parseBegin = toString <$> satisfy isBegin

parseEnd :: Parser Token String
parseEnd = toString <$> satisfy isEnd

isVersion :: Token -> Bool
isVersion (TVersion _) = True
isVersion _            = False

parseProdID :: Parser Token String
parseProdID = toString <$> satisfy isProdId

isProdId :: Token -> Bool
isProdId (TProdid _) = True
isProdId _           = False


-- EVENT PARSING

toEvent :: Parser Token VEvent
toEvent = f <$> toEvent'
  where f ((DateTimeProp x _):(StringProp y _):(DateTimeProp z _):(DateTimeProp a _):(MaybeStringProp b _):(MaybeStringProp c _):[(MaybeStringProp d _)]) = VEvent x y z a b c d

toEvent' :: Parser Token [EventProp]
toEvent' = do f <- some parseProp
              x <- check f
              return (sortBy keysort x)

keysort a b | (f a) < (g b) = LT
            | (f a) == (g b) = EQ
            | otherwise = GT
              where f (DateTimeProp _ k) = k
                    f (StringProp _ k) = k
                    f (MaybeStringProp _ k) = k
                    g (DateTimeProp _ k) = k
                    g (StringProp _ k) = k
                    g (MaybeStringProp _ k) = k

check :: [EventProp] -> Parser Token [EventProp]
check xs | notElem Description ts = check ((xs ++ [(MaybeStringProp Nothing Description)]))
         | notElem Location ts   =     check ((xs ++ [(MaybeStringProp Nothing Location)]))
         | notElem Summary ts =     check ((xs ++ [(MaybeStringProp Nothing Summary)]))
         | elem RestKey ts = check (take (fromJust (elemIndex RestKey ts)) xs ++ drop (1 + (fromJust (elemIndex RestKey ts))) xs)
         | otherwise = succeed xs
               where ts = map f xs
                     f (DateTimeProp y z) = z
                     f (StringProp y z) = z
                     f (MaybeStringProp y z) = z

 -- parseDTE <*> parseUID <* option parseRest "" <*> parseDT <*> parseDT <*> optional parseDesc <* option parseRest "" <*> optional parseSummary <* option parseRest "" <*> optional parseLocation <* option parseRest "" <* (satisfy isEnd)

data Key = DTStamp | UID | DTStart | DTEnd | Description | Summary | Location | RestKey
  deriving (Eq, Ord, Show)

data EventProp = DateTimeProp    DateTime       Key
               | StringProp      String         Key
               | MaybeStringProp (Maybe String) Key
               deriving (Eq, Ord, Show)

isEEnd :: Token -> Bool
isEEnd (TDTEnd _) = True
isEEnd (_) = False

parseProp :: Parser Token EventProp
parseProp = choice [parseUID, parseDesc, parseSummary, parseLocation, parseRest, parseDTE, parseRest]

parseUID :: Parser Token EventProp
parseUID = toStringE <$> satisfy isUID

toStringE :: Token -> EventProp
toStringE (TUID x)         = StringProp x UID
toStringE (TSummary x)     = MaybeStringProp (Just x) Summary
toStringE (TDescription x) = MaybeStringProp (Just x) Description
toStringE (TLocation x)    = MaybeStringProp (Just x) Location
toStringE (Rest)         = StringProp "" RestKey
-- toStringE Rest             = StringProp ""

isUID :: Token -> Bool
isUID (TUID _) = True
isUID _        = False

parseDTE :: Parser Token EventProp
parseDTE = toDTE <$> satisfy isDt

toDTE :: Token -> EventProp
toDTE (TDTStamp x) = DateTimeProp (fst (head (parse parseDateTime x))) DTStamp
toDTE (TDTStart x) = DateTimeProp (fst (head (parse parseDateTime x))) DTStart
toDTE (TDTEnd x)   = DateTimeProp (fst (head (parse parseDateTime x))) DTEnd
toDTE _            = error "TimeStamp"

isDt :: Token -> Bool
isDt (TDTStamp _) = True
isDt (TDTStart _) = True
isDt (TDTEnd _)   = True
isDt _            = False

isBegin :: Token -> Bool
isBegin (TBegin _)   = True
isBegin _            = False

isEvent :: Token -> Bool
isEvent (TEnd _) = False
isEvent _        = True

isEnd :: Token -> Bool
isEnd (TEnd _) = True
isEnd _        = False

parseDesc :: Parser Token EventProp
parseDesc = toStringE <$> satisfy isDesc

isDesc :: Token -> Bool
isDesc (TDescription _) = True
isDesc _                = False

parseSummary :: Parser Token EventProp
parseSummary = toStringE <$> satisfy isSummary

isSummary :: Token -> Bool
isSummary (TSummary _) = True
isSummary _            = False

parseLocation :: Parser Token EventProp
parseLocation = toStringE <$> satisfy isLocation

isLocation :: Token -> Bool
isLocation (TLocation _) = True
isLocation _             = False

parseRest :: Parser Token EventProp
parseRest = toStringE <$> satisfy isRest

isRest :: Token -> Bool
isRest Rest = True
isRest _    = False

toString :: Token -> String
toString (TProdid x)      = x
toString Rest             = ""

-- Exercise 2
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar p = do f <- openFile p ReadMode
                    d <- hGetContents f
                    return (recognizeCalendar d)

-- Exercise 3
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar (Calendar p e) =
  "BEGIN:VCALENDAR\r\n"     ++
  "PRODID:" ++ p ++ "\r\n"  ++
  "VERSION:2.0" ++ "\r\n"   ++
  concatMap printEvent e    ++
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
checkOverlapping (Calendar _ e) | length e <= 2 = overlapping [e]
                                | otherwise =  overlapping (uniqueComb 2 e)

overlapping :: [[VEvent]] -> Bool
overlapping []     = False
overlapping (x:xs) = o h t || overlapping xs
  where h = head x
        t = last x
        o (VEvent _ _ start1 end1 _ _ _) (VEvent _ _ start2 end2 _ _ _) | (start1 < end2 && start2 < end1) || (end1 == start2) || (start1 == end2) = True
                                                                        | otherwise = False

uniqueComb :: (Eq t, Num t) => t -> [a] -> [[a]]
uniqueComb 0 _  = [[]]
uniqueComb _ [] = []
uniqueComb k as = [x:xs | (x:as') <- tails as, xs <- uniqueComb (k-1) as']

timeSpent :: String -> Calendar -> Int
timeSpent f (Calendar p e) = sum [time' end start | ev@(VEvent _ _ start end _ sum' _) <- e, Just f == sum']
  where time' (DateTime (Date (Year y1) (Month mon1) (Day d1)) (Time (Hour h1) (Minute min1) (Second s1)) _) (DateTime (Date (Year y2) (Month mon2) (Day d2)) (Time (Hour h2) (Minute min2) (Second s2)) _) = dateToMinutes (subtractDates (timeToDays h1 min1 s1 + dateToDays y1 mon1 d1) (timeToDays h2 min2 s2 + dateToDays y2 mon2 d2))

-- Calculate no. days from date and time for two datetimes, subtract and convert to minues.

dateToDays :: Int -> Int -> Int -> Double
dateToDays y m d = toEnum (floor (365 * y' + y' / 4 - y' / 100 + y' / 400 + d' + (153 * m' + 8) / 5))::Double
  where y' | m <= 2    = toEnum (y - 1)::Double
           | otherwise = toEnum y::Double
        m' | m <= 2    = toEnum (m + 12)::Double
           | otherwise = toEnum m::Double
        d' = toEnum d::Double

timeToDays :: Int -> Int -> Int -> Double
timeToDays h m s = (h' / 24) + (m' / 60 / 24) + (s' / 60 / 60 / 24)
  where h' = toEnum h::Double
        m' = toEnum m::Double
        s' = toEnum s::Double

subtractDates :: Double -> Double -> (Double, Double, Double, Double)
subtractDates date1 date2 = (days, hours, minutes, seconds)
  where sum  = date1 - date2
        days = floor' sum
        hours = floor' ((sum - days) * 24)
        minutes = floor' ((((sum - days) * 24) - hours) * 60)
        seconds = round' ((((((sum - days) * 24) - hours) * 60) - minutes) * 60)

floor' :: Double -> Double
floor' d = toEnum (floor d)::Double

round' :: Double -> Double
round' d = toEnum (round d)::Double

dateToMinutes :: (Double, Double, Double, Double) -> Int
dateToMinutes (d, h, m, s) = fromEnum ((d * 24 * 60) + (h * 60) + m + (s / 60))::Int

-- Exercise 5
ppMonth :: Year -> Month -> Calendar -> String
ppMonth (Year y) (Month m) (Calendar _ e) = render (view' [week1, sep, week2, sep, week3, sep, week4, sep, week5])
  where days  = getDay (Year y) (Month m)
        week1 = row' [getEvents y m x e | x <- [1..7]]
        week2 = row' [getEvents y m x e | x <- [8..14]]
        week3 = row' [getEvents y m x e | x <- [15..21]]
        week4 = row' [getEvents y m x e | x <- [22..28]]
        week5 | days > 28 = row' [getEvents y m x e | x <- [29..days]]
              | otherwise = nullBox
        sep  = text (concat (replicate (14 + (16 * 6)) "-"))

getEvents :: Int -> Int -> Int -> [VEvent] -> Box
getEvents y m d e = box' (show d) [displayTime (d==d1) ti1 (d==d2) ti2 | (VEvent _ _ (DateTime da1@(Date (Year y1) (Month mo1) (Day d1)) ti1 _) (DateTime da2@(Date (Year y2) (Month mo2) (Day d2)) ti2 _) _ _ _) <- e, y1 == y && mo1 == m && (d == d1 || d == d2)]

displayTime :: Bool -> Time -> Bool -> Time -> String
displayTime d1 (Time (Hour h1) (Minute m1) _) d2 (Time (Hour h2) (Minute m2) _) = overlap d1 h1 m1 ++ " - " ++ overlap d2 h2 m2
  where n x | x == 0    = "00"
            | x < 10    = "0" ++ show x
            | otherwise = show x
        overlap d h m | d         = n h ++ ":" ++ n m
                      | otherwise = "*"

box' :: String -> [String] -> Box
box' d e = vcat left (text (d ++ concat (replicate (14 - length d) " ")) : [text x | x <- e])

row' :: [Box] -> Box
row'= hsep' left

view' :: [Box] -> Box
view' = vcat left

hsep' :: Alignment -> [Box] -> Box
hsep' a = punctuateH a (text "| ")
