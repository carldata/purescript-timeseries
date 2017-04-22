-- | Helper function for DateTime operations
module Data.TimeSeries.Time.Parser ( parseISOTime ) where

import Data.Maybe (Maybe(..))
import Global (isNaN)


foreign import parseISOTimeImpl :: String -> Number


parseISOTime :: String -> Maybe Number 
parseISOTime str = if isNaN ts then Nothing else Just ts
  where ts = parseISOTimeImpl str
