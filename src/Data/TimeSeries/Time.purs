-- | Helper function for DateTime operations
module Data.TimeSeries.Time where

import Prelude
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds(..))


-- Convert timestamp in seconds into the DateTime
fromSeconds :: Number -> DateTime
fromSeconds n = toDateTime ts
  where 
    ms = Milliseconds (1000.0 * n)
    ts = fromMaybe bottom (instant ms)
