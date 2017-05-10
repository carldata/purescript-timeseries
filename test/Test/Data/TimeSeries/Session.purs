module Test.Data.TimeSeries.Session (testSessions) where

import Prelude
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Test.Assert (assert, ASSERT)

import Data.TimeSeries.Time (seconds)
import Data.TimeSeries as TS
import Data.TimeSeries.Session as S


testSessions :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION  | eff) Unit
testSessions = do

  log "\n# Session tests"
  noBreaksTest
  multiSessionsTest
  multiEventsTest

    
noBreaksTest :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION  | eff) Unit
noBreaksTest = do
  log "* Without breaks"
  let xs = TS.fromValues [1.0, 2.0, 3.0, 2.0, 1.0, 0.0]
  assert $ S.find (seconds 1) xs == [S.session 0.0 5000.0]


multiSessionsTest :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION  | eff) Unit
multiSessionsTest = do
  log "* 2 sessions"
  let xs = TS.fromValues [0.0, 1.0, 2.0, 3.0, 0.0, 0.0, 1.0, 0.0]
  assert $ S.find (seconds 1) xs == [S.session (seconds 1) (seconds 4), S.session (seconds 6) (seconds 7)]


multiEventsTest :: ∀ eff. Eff (console :: CONSOLE, assert :: ASSERT, exception :: EXCEPTION  | eff) Unit
multiEventsTest = do
  log "* Single session 2 events"
  let xs = TS.fromValues [0.0, 1.0, 2.0, 0.0, 3.0, 4.0, 0.0, 0.0]
  assert $ S.find (seconds 2) xs == [S.session (seconds 1) (seconds 6)]
