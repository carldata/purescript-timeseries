-- | Helper function for DateTime operations
module Data.TimeSeries.Time (Timestamp, fromSeconds) where

import Prelude
import Data.Int (toNumber)


-- Time stamp in milliseconds as defined by JS Date function
type Timestamp = Number

-- | Convert Timestamp in seconds into the DateTime
fromSeconds :: Int -> Timestamp
fromSeconds n = toNumber (1000 * n)
