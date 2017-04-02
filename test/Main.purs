module Test.Main where

import Prelude (Unit)
import Signal.Channel (CHANNEL)
import Node.Process (PROCESS)
import Control.Monad.ST (ST)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Timer (TIMER)
import Test.Signal.TimeTravel (main) as TimeTravel
import Test.Signal.TimeTravel.Mock (Action, State)

main
 ∷ Eff ( console ∷ CONSOLE
       , timer ∷ TIMER
       , avar ∷ AVAR
       , process ∷ PROCESS
       , channel ∷ CHANNEL
       , st ∷ ST { action ∷ Action, state ∷ State }
       ) Unit
main = TimeTravel.main
