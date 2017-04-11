-- | Anomaly detection in Time Series
-- | Current implementation looks for outliers
module Data.TimeSeries.Anomaly 
  ( Model
  , learn
  , predict
  ) where


import Data.TimeSeries as TS

-- | Anomaly detection model
data Model = Model

-- Convert timestamp in seconds into the DateTime
learn :: ∀ a. TS.Series a -> Model
learn xs = Model


-- | Predict values based on learned model
predict :: ∀ a. Model -> TS.Series a -> TS.Series a
predict model xs = xs