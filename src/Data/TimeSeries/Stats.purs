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


-- Calculate mean
mean :: TS.Series Number -> Number
mean xs = sum (TS.values xs) / toNumber (TS.length xs)


-- Calculate variance
variance :: TS.Series Number -> Number
variance xs = sum (map (\v -> pow (v-mu) 2.0) vs) / toNumber (TS.length xs)
  where vs = TS.values xs
        mu = mean xs


-- Calculate standard deviation
stddev :: TS.Series Number -> Number
stddev = sqrt <<< variance
