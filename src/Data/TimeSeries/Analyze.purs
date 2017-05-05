-- | Analyze points in the Time Series
module Data.TimeSeries.Analyze (findMissing, findMissing') where


import Prelude 
import Data.Array as A
import Data.Maybe (fromMaybe)

import Data.TimeSeries as TS 
import Data.TimeSeries.Time (Timestamp)


-- | Find missing points in time series.
-- | Points are estimated from provided time resolution
findMissing :: ∀ a. TS.Series a -> Timestamp -> Array Timestamp
findMissing xs dt = findMissing' si dt (TS.index xs) []
    where 
        si = fromMaybe 0.0 $ TS.dpIndex <$> TS.head xs

-- Helper function operating on Array of Timestamp
findMissing' :: Timestamp          -- ^ Previous Timestamp
             -> Timestamp          -- ^ Time delta
             -> Array Timestamp    -- ^ Imput array with timestamp from the series
             -> Array Timestamp    -- ^ Partial output array. Build so far
             -> Array Timestamp    -- ^ Output
findMissing' _ _ [] out = out
findMissing' idx dt xs out = 
  if idx < x then findMissing' nidx dt xs (A.snoc out idx)
  else if idx == x then findMissing' nidx dt ts out
  else findMissing' idx dt ts out
    where
      x = fromMaybe 0.0 $ A.head xs
      ts = fromMaybe [] $ A.tail xs
      nidx = idx + dt
