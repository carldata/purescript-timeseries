-- | Helper function for DateTime operations
module Data.TimeSeries.Time.Parser ( parseISODateTime ) where

import Prelude
import Data.Array (some)
import Data.Char.Unicode (isDigit)
import Data.Date (Date, canonicalDate)
import Data.DateTime (DateTime(..), Time)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Int (fromString, pow)
import Data.Maybe (fromMaybe)
import Data.String (fromCharArray, length)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.String (satisfy)


-- Tuple definition
data Tuple a b = Tuple a b


-- | Parse ISO date. If date can't be parsed then this function will return the lowest date
parseISODateTime :: String -> DateTime
parseISODateTime str =
    case parseISOTimeOrError str of
        Left _ -> bottom
        Right dt -> dt


-- | Parse date and return error if date can't be parsed.
-- | If you need safe parser then there is a function: parseISODateTime
parseISOTimeOrError :: String -> Either ParseError DateTime
parseISOTimeOrError str = runParser str isoDateTime


-- Create UTCTime parser for ISO date time
-- This code is based on code from sqlite-simple package
isoDateTime :: Parser String DateTime
isoDateTime = do
    d  <- isoDate
    -- time <- ((char ' ' <|> char 'T') *> isoTime) <|> pure midnight
    let time' = bottom :: Time -- let time' = timeOfDayToTime time
    pure (DateTime d time')


isoDate :: Parser String Date
isoDate = do
    year <- int
    month <- pure 1 -- month <- (char '-' *> digits "month") <|> pure 1
    day   <- pure 1 -- day   <- (char '-' *> digits "day") <|> pure 1
    pure $ fromMaybe bottom $ canonicalDate <$> toEnum year <*> toEnum month <*> toEnum day


-- isoTime :: Parser TimeOfDay
-- isoTime = do
--     hour   <- digits "hours"
--     _      <- char ':'
--     minute <- digits "minutes"
--     -- Allow omission of seconds.  If seconds is omitted, don't try to
--     -- parse the sub-second part.
--     Tuple sec subsec
--            <- (Tuple <$> (char ':' *> digits "seconds") <*> fract) <|> pure Tuple 0 0

--     let picos' = sec + subsec

--     case makeTimeOfDayValid hour minute picos' of
--       Nothing -> fail "invalid time of day"
--       Just x  -> return $! x

--     where
--       fract =
--         (char '.' *> (decimal <$> A.takeWhile1 isDigit)) <|> pure 0



int :: Parser String Int
int = do
  xn <- some digit
  pure $ readInt xn


-- decimal :: String -> Number
-- decimal str = 
--   let d = readInt str
--       n = pow 10 (length str)
--   in d / n

-- | Match digit
digit :: Parser String Char
digit = satisfy isDigit

-- Helper function for reading Int value from String
-- If the value is wrong formatted then 0 is returned
readInt :: Array Char -> Int 
readInt str = fromMaybe 0 $ fromString $ fromCharArray str
