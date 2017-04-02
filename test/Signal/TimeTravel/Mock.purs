module Test.Signal.TimeTravel.Mock where

import Prelude ((+), (-), (<>), (&&), (==), show, class Eq, class Show)

type State = Int
data Action = Noop | Increment | Decrement

initialState ∷ State
initialState = 0

instance showAction ∷ Show Action where
  show Noop = "Noop"
  show Increment = "Increment"
  show Decrement = "Decrement"

instance eqAction ∷ Eq Action where
  eq Noop Noop = true
  eq Increment Increment = true
  eq Decrement Decrement = true
  eq _ _ = false

newtype Crumb action state = Crumb { action ∷ action, state ∷ state }

instance showCrumb ∷ (Show action, Show state) ⇒ Show (Crumb action state) where
  show (Crumb { action, state }) = "{ action: " <> show action <> ", state: " <> show state <> "}"

instance eqCrumb ∷ (Eq action, Eq state) ⇒ Eq (Crumb action state) where
  eq (Crumb { action, state }) (Crumb actionState) = action == actionState.action && state == actionState.state

update ∷ Action → State → State
update Noop      state = state
update Increment state = state + 1
update Decrement state | state == 0 = state
update Decrement state = state - 1
