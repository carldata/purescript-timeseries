-- | Find and fix anomalies in Time Series
module Data.TimeSeries.Anomalies (Model, train, removeOutliers) where

import Prelude
import Data.Array as A 
import Data.Maybe (fromMaybe)
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
removeOutliers :: TS.Series Number -> Model -> TS.Series Number
removeOutliers xs m = xs
