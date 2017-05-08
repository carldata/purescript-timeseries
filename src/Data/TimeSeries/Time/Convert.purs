-- | Helper function for DateTime operations
module Data.TimeSeries.Time.Convert ( parseISOTime, formatTime ) where

import Data.Maybe (Maybe(..))
import Global (isNaN)


foreign import parseISOTimeImpl :: String -> Number
-- | Convert Timestamp into date representation
foreign import formatTime :: Number -> String

-- | Parse string representation of date time into the Timestamp.
parseISOTime :: String -> Maybe Number 
parseISOTime str = if isNaN ts then Nothing else Just ts
  where ts = parseISOTimeImpl str


