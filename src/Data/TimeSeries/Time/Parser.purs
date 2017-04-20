-- | Helper function for DateTime operations
module Data.TimeSeries.Time.Parser ( parseISOTime ) where

import Prelude
import Data.Date (canonicalDate)
import Data.DateTime (DateTime(..), Time(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe)


type DateRec =
  { year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  , second :: Int
  , millisecond :: Int
  }

foreign import parseIsoNative :: String -> DateRec


-- | Parse ISO date. Date can be provided in partial form.
-- | E.g. All forms '2015', '2015-02-04', '2015;-5-12T12:45:67" are correct"
parseISOTime :: String -> Maybe DateTime
parseISOTime str = fromDateRec $ parseIsoNative str

-- Convert DateRec from JS
fromDateRec :: DateRec -> Maybe DateTime
fromDateRec rec = do
    d <- canonicalDate <$> toEnum rec.year <*> toEnum rec.month <*> toEnum rec.day
    t <- Time <$> toEnum rec.hour <*> toEnum rec.minute <*> toEnum rec.second <*> toEnum rec.millisecond
    pure $ DateTime d t 
