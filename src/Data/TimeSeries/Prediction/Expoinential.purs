module Data.TimeSeries.Prediction.Exponential where 


-- | Model for Exponential Smoothing.
-- | This model keeps alpha coefficient and last value
-- | New value for thi model can be predicted with formula:
-- | x[t+1] = alphax[t] + (1-alpha)*l
-- | where l is cummulative value
data Model = Model Number Number  -- ^ alpha and cummulative value


-- | Simple Exponential Smoothing
ses :: Number -> Series Number -> Model 
ses a xs = Model a 0.0 


-- | Update model with new value from Time Series.
-- | Online learning 
update :: Model -> Number -> Model 
update m d = m 


-- | Predict next n values
predict :: Int -> Model -> Number 
predict n m = 0.0
