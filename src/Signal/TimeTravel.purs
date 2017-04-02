module Signal.TimeTravel ( initialize ) where

import Prelude (Unit, bind, pure, unit, ($), (&&), (+), (-), (/=), (==))
import Signal.Channel (CHANNEL, Channel)
import Signal.Channel (send) as Channel
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef, writeSTRef)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Array.ST (STArray, emptySTArray, freeze, pushSTArray)
import Data.Array (length, (!!))
import Data.Maybe (Maybe(..), maybe)


increment ∷ Int → Int → Int
increment max value | value == max && max /= 0 = value
increment _   value = value + 1


decrement ∷ Int → Int
decrement value | value == 0 = 0
decrement value = value - 1


next
  ∷ ∀ effects action state
  . STArray {action ∷ action, state ∷ state} {action ∷ action, state ∷ state}
  → STRef {action ∷ action, state ∷ state} Int
  → STRef {action ∷ action, state ∷ state} Boolean 
  → Channel action
  → Eff ( st ∷ ST {action ∷ action, state ∷ state}
        , channel ∷ CHANNEL
        | effects
        ) Unit
next breadcrumb' pointer sentAction channel = do
  breadcrumb ← freeze breadcrumb'
  modifySTRef pointer (increment ((length breadcrumb) - 1))
  index ← readSTRef pointer
  case breadcrumb !! index of
    Just { action, state } → do
      writeSTRef sentAction true
      Channel.send channel action
    Nothing → pure unit


prev
  ∷ ∀ effects action state
  . STArray {action ∷ action, state ∷ state} {action ∷ action, state ∷ state}
  → STRef {action ∷ action, state ∷ state} Int
  → STRef {action ∷ action, state ∷ state} Boolean 
  → Channel action
  → Eff ( st ∷ ST {action ∷ action, state ∷ state}
        , channel ∷ CHANNEL
        | effects
        ) Unit
prev breadcrumb' pointer sentAction channel = do
  breadcrumb ← freeze breadcrumb'
  modifySTRef pointer decrement
  index ← readSTRef pointer
  case breadcrumb !! index of
    Just { action, state: _ } → do
      writeSTRef sentAction true
      Channel.send channel action
    Nothing → pure unit


capture
  ∷ ∀ action state
  . STArray {action ∷ action, state ∷ state} {action ∷ action, state ∷ state}
  → STRef {action ∷ action, state ∷ state} Int
  → STRef {action ∷ action, state ∷ state} Boolean 
  → (action → state → state)
  → action
  → state
  → state
capture breadcrumb pointer sentAction update action state = unsafePerformEff $ do
  breadcrumb' ← freeze breadcrumb 
  pointer' ← readSTRef pointer
  sentAction' ← readSTRef sentAction

  case sentAction' of
    true → do
      -- | Reset sentAction ref
      writeSTRef sentAction false

      pure $ maybe state (\{ state } → state) $ breadcrumb' !! pointer'
    false → do
      -- | Reset sentAction and pointer refs
      writeSTRef sentAction false
      writeSTRef pointer (length breadcrumb')

      let newState = update action state
      pushSTArray breadcrumb { action, state: newState }
      pure newState


read
  ∷ ∀ action state effects
  . STArray {action ∷ action, state ∷ state} {action ∷ action, state ∷ state}
  → STRef {action ∷ action, state ∷ state} Int
  → Eff
    effects
    { breadcrumb ∷ STArray {action ∷ action, state ∷ state} {action ∷ action, state ∷ state}
    , pointer ∷ STRef {action ∷ action, state ∷ state} Int
    }
read breadcrumb pointer = pure { breadcrumb, pointer }


initialize
  ∷ ∀ action state effects effects'
  . Channel action
  → (action → state → state)
  → Eff ( st ∷ ST {action ∷ action, state ∷ state}
        , channel ∷ CHANNEL
        | effects
        )
        { send ∷ action → Eff (channel ∷ CHANNEL | effects) Unit
        , prev ∷ Eff (st ∷ ST {action ∷ action, state ∷ state}, channel ∷ CHANNEL | effects) Unit
        , next ∷ Eff (st ∷ ST {action ∷ action, state ∷ state}, channel ∷ CHANNEL | effects) Unit
        , read ∷ Eff effects' { breadcrumb ∷ STArray {action ∷ action, state ∷ state} {action ∷ action, state ∷ state}
                              , pointer ∷ STRef {action ∷ action, state ∷ state} Int
                              }
        , update ∷ action → state → state
        }
initialize channel update = do
  breadcrumb ← emptySTArray
  pointer ← newSTRef 0

  -- | This is a flag gets set to true whenever an action is sent from `prev` and `next` to communicate to the capture
  -- | function that the sent action is coming from the time travel debugger
  sentAction ← newSTRef false

  pure { send: Channel.send channel
       , prev: prev breadcrumb pointer sentAction channel
       , next: next breadcrumb pointer sentAction channel
       , read: read breadcrumb pointer
       , update: capture breadcrumb pointer sentAction update
       }
