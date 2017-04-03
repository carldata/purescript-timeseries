module Data.TimeSeries where

import Data.Array as Array
import Data.DateTime (DateTime)


-- | Data points is a time indexed value
data DataPoint a = 
    DP { dpIndex :: DateTime     -- ^ Get data point index.
       , dpValue :: a            -- ^ Get data point value.
       }
                 
-- | Data structure for holding Series.
data Series a = Series (Array (DataPoint a))


-- | Create empty series
empty :: ∀ a. Series a
empty = Series []

-- | Create series from UTCTime and value.
series :: ∀ a. Array (DataPoint a) -> Series a
series xs = Series xs


-- | Get series size.
--
-- >size (Series [DP 1 41.3, DP 2 52.22, DP 3 3.0]) == 3
--
length :: ∀ a. Series a -> Int
length (Series xs) = Array.length xs
