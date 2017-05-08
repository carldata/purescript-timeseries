-- | Find and fix anomalies in Time Series
module Data.TimeSeries.Anomaly (Model, train, removeOutliers) where

import Prelude
import Data.Array as A 
import Data.Maybe (fromMaybe)
import Learn.Unsupervised.OutlierDetection as OD
import LinearAlgebra.Matrix as M
import Statistics.Sample as S

import Data.TimeSeries as TS 


data Model = Model OD.Model Number -- Model and training mean as default value

instance showModel :: Show Model where
  show (Model ds _) = show ds


-- | Train Anomaly detection model
train :: TS.Series Number -> Model
train xs = Model (OD.train mat) (S.mean vs)
  where 
    vs = TS.values xs
    mat = fromMaybe (M.zeros 1 1) $ M.fromArray (A.length vs) 1 vs


-- | Remove outliers from the series
-- | If given point has high probability of being outlier then 
-- | its value is replaced by value of the previous point.
removeOutliers :: Model -> TS.Series Number -> TS.Series Number
removeOutliers (Model model mu) xs = TS.series (TS.index xs) (A.zipWith f vs ys)
  where
    vs = TS.values xs
    ys = predictOutliers model xs
    f x p = if p < 0.01 then mu else x

-- Find outliers in time series.
-- Return probabilites of being outlier
predictOutliers :: OD.Model -> TS.Series Number -> Array Number
predictOutliers model xs = OD.predict model vs'
  where
    vs = TS.values xs
    vs' = fromMaybe (M.zeros 1 1) $ M.fromArray (A.length vs) 1 vs
