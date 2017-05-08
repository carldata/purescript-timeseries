-- | Find and fix anomalies in Time Series
module Data.TimeSeries.Anomaly (Model, train, removeOutliers) where

import Prelude
import Data.Array as A 
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Learn.Unsupervised.OutlierDetection as OD
import LinearAlgebra.Matrix as M

import Data.TimeSeries as TS 


data Model = Model OD.Model

-- | Train Anomaly detection model
train :: TS.Series Number -> Model
train xs = Model $ OD.train ys
  where 
    vs = TS.values xs
    ys = fromMaybe (M.zeros 1 1) $ M.fromArray (A.length vs) 1 vs


-- | Remove outliers from the series
-- | If given point has high probability of being outlier then 
-- | its value is replaced by value of the previous point.
removeOutliers :: Model -> TS.Series Number -> TS.Series Number
removeOutliers model xs = TS.series (TS.index xs) (A.zipWith f vs ys2)
  where
    vs = TS.values xs
    v = fromMaybe 0.0 $ A.head vs
    ys = predictOutliers model xs
    ys2 = A.zip ys (A.cons v vs)
    f :: Number -> Tuple Number Number -> Number 
    f x (Tuple p y) = if p < 0.01 then y else x

-- Find outliers in time series.
-- Return probabilites of being outlier
predictOutliers :: Model -> TS.Series Number -> Array Number
predictOutliers (Model model) xs = OD.predict model vs'
  where
    vs = TS.values xs
    vs' = fromMaybe (M.zeros 1 1) $ M.fromArray (A.length vs) 1 vs
