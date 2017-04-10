-- | Helper function for DateTime operations
module Data.TimeSeries.Time.Parser ( parseISOTime, parseISOTimeOrError ) where

import Prelude
import Control.Alt ((<|>))
import Data.Array (some)
import Data.Char.Unicode (isDigit)
import Data.Date (Date, canonicalDate)
import Data.DateTime (DateTime(..), Time(..))
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (fromCharArray)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.String (char, satisfy)



-- | Parse ISO date. If date can't be parsed then this function will return the lowest date (bottom)
parseISOTime :: String -> Maybe DateTime
parseISOTime str =
    case parseISOTimeOrError str of
        Left _ -> Nothing
        Right dt -> Just dt


-- Parse date and return error if date can't be parsed.
parseISOTimeOrError :: String -> Either ParseError DateTime
parseISOTimeOrError str = runParser str isoDateTime


-- ISO DateTime parser
isoDateTime :: Parser String DateTime
isoDateTime = do
    d    <- isoDate
    time <- ((char ' ' <|> char 'T') *> isoTime) <|> pure bottom
    pure (DateTime d time)


-- Parse Date part
isoDate :: Parser String Date
isoDate = do
    year  <- int
    month <- (char '-' *> int) <|> pure 1
    day   <- (char '-' *> int) <|> pure 1
    pure $ fromMaybe bottom $ canonicalDate <$> toEnum year <*> toEnum month <*> toEnum day


-- Time if present requires at least hour and minutes. Seconds are optional
isoTime :: Parser String Time
isoTime = do
    hour   <- int
    _      <- char ':'
    minute <- int
    sec    <- (char ':' *> int) <|> pure 0
    pure $ fromMaybe bottom $ Time <$> toEnum hour <*> toEnum minute <*> toEnum sec <*> toEnum 0


-- Parser for Int type
-- This parser will only parse positive numbers (without '-' sign)
int :: Parser String Int
int = readInt <$> some digit


-- decimal :: String -> Number
-- decimal str = 
--   let d = readInt str
--       n = pow 10 (length str)
--   in d / n

-- Single digit parser
digit :: Parser String Char
digit = satisfy isDigit

-- Helper function for reading Int value from Array of Chars
-- If the value is wrong formatted then 0 is returned
readInt :: Array Char -> Int 
readInt str = fromMaybe 0 $ fromString $ fromCharArray str
