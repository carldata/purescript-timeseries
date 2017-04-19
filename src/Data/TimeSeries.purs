module Data.TimeSeries 
    ( DataPoint
    , Series
    , dataPoint
    -- * Constructors
    , empty
    , fromDataPoints
    , fromValues
    , series
    , mkIndex
    -- Data access
    , length
    , slice
    ) where

import Prelude
import Data.Array as A
import Data.TimeSeries.Time as T
import Data.DateTime (DateTime)
import Data.Maybe (fromMaybe)


-- | Data points is a time indexed value
type DataPoint a = { index :: DateTime, value :: a }
                 
-- | Data structure for holding Series.
type Series a = { index :: Array DateTime  
                , values :: Array a 
                }


-- Create data point
dataPoint :: ∀ a. DateTime -> a -> DataPoint a
dataPoint i v = { index: i, value: v }


--  | Create series from indexes and values
series :: ∀ a. Array DateTime -> Array a -> Series a 
series is vs = {index: is, values: vs}


-- | Create an empty series
empty :: ∀ a. Series a
empty = series [] []


-- | Create series from DateTime and value.
fromDataPoints :: ∀ a. Array (DataPoint a) -> Series a
fromDataPoints xs = series (map _.index xs) (map _.value xs)


-- | Create series from values.
-- | Index will be based on Instant starting from 0
fromValues :: ∀ a. Array a -> Series a
fromValues xs = series (mkIndex (A.length xs)) xs
    

-- Create index starting from lowest date (bottom)
-- In each step time is increased by 1 second
mkIndex :: Int -> Array DateTime
mkIndex n = map T.fromSeconds (A.range 1 n)


-- | Get series length.
length :: ∀ a. Series a -> Int
length xs = A.length xs.index


-- | Get subseries
slice :: ∀ a. DateTime  -- ^ Start time (inclusive)
      -> DateTime       -- ^ End time (inclusive)
      -> Series a       -- ^ Input series
      -> Series a       -- ^ Sliced Series
slice start end xs = series (A.slice i j xs.index) (A.slice i j xs.values)
    where 
        n = length xs
        i = fromMaybe n $ A.findIndex (\x -> x >= start) xs.index
        j = fromMaybe n $ A.findLastIndex (\x -> x <= end) xs.index
