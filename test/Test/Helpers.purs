module Test.Helpers ( NOW, now ) where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)



-- | Effect type for when accessing the current date/time.
foreign import data NOW :: Effect

-- | Gets an `Instant` value for the date and time according to the current
-- | machine’s clock.
foreign import now :: forall e. Eff (now :: NOW | e) Number

