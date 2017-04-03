module Data.TimeSeries where

import Prelude

  
-- | Data points is a time indexed value
data DataPoint a = 
    DP { dpIndex :: Int          -- ^ Get data point index.
       , dpValue :: a            -- ^ Get data point value.
       }
                 
