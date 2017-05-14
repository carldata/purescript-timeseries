module Data.TimeSeries.Model.Exponential 
    ( Model
    , ses
    , fromSeries
    , update
    , predictNext
    ) where 

import Prelude
import Data.Array as A 

import Data.TimeSeries as TS 


-- | Model for Exponential Smoothing.
-- | This model keeps alpha coefficient and last value
-- | New value for thi model can be predicted with formula:
-- | x[t+1] = alphax[t] + (1-alpha)*l
-- | where l is cummulative value
data Model = Model Number Number  -- ^ alpha and cummulative (next) value

instance eqModel :: Eq Model where 
    eq (Model a1 c1)  (Model a2 c2) = a1 == a2 && c1 == c2


-- | Initialize Simple Exponential Smoothing model
-- | alpha should be inrange [0, 1]
ses :: Number   -- ^ alpha value
    -> Number   -- ^ Initial value
    -> Model 
ses a v = Model a' v 
    where a' = if a <= 1.0 && a >= 0.0 then a else 1.0


-- | Create model from data. 
-- | Only 10 last data points will be uses to build model
fromSeries :: Number -> TS.Series Number -> Model
fromSeries alpha xs = A.foldl update model ds 
    where 
        model = ses alpha 0.0
        ds =  A.take 10 <<< A.reverse $ TS.values xs


-- | Update model with new value from Time Series.
-- | Online learning 
update :: Model -> Number -> Model 
update (Model a c) d = Model a c' 
    where 
        c' = a*d + (1.0-a)*c


-- | Predict next value
predictNext :: Model -> Number 
predictNext (Model _ c) = c
