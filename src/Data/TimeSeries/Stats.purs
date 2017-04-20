-- | Calculate TimeSeries statistics
module Data.TimeSeries.Stats 
  ( mean
  , stddev
  , variance
  ) where

import Prelude
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.TimeSeries as TS
import Math (pow, sqrt)


-- Calculate mean of series values
mean :: TS.Series Number -> Number
mean xs = sum xs.values / toNumber (TS.length xs)


-- Calculate variance of series values
variance :: TS.Series Number -> Number
variance xs = sum (map (\v -> pow (v-mu) 2.0) xs.values) / toNumber (TS.length xs)
  where 
    mu = mean xs


-- Calculate standard deviation
stddev :: TS.Series Number -> Number
stddev = sqrt <<< variance
