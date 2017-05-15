-- | Helper function for DateTime operations
module Data.TimeSeries.Time 
  ( Timestamp
  , parseISOTime
  , formatTime
  , seconds
  , minutes
  , hours
  ) where

import Prelude
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Global (isNaN)


-- | Time stamp in milliseconds as defined by JS Date function
type Timestamp = Number


foreign import parseISOTimeImpl :: String -> Number
-- | Convert Timestamp into date representation
foreign import formatTime :: Number -> String


-- | Parse string representation of date time into the Timestamp.
parseISOTime :: String -> Maybe Number 
parseISOTime str = if isNaN ts then Nothing else Just ts
  where ts = parseISOTimeImpl str
  
  
-- | Convert given number of seconds into Timestamp value.
seconds :: Int -> Timestamp
seconds n = toNumber (1000 * n)

-- | Convert given number of minutes into Timestamp value.
minutes :: Int -> Timestamp
minutes n = toNumber (60000 * n)

-- | Convert given number of hours into Timestamp value.
hours :: Int -> Timestamp
hours n = toNumber (60*60*1000 * n)

