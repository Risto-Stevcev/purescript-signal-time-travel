module Test.Signal.TimeTravel where

import Prelude (Unit, bind, pure, unit, ($), (-), (<$>), (<*))
import Signal (runSignal, foldp, (~>))
import Signal.Channel (CHANNEL)
import Signal.Channel (channel, subscribe, send) as Channel
import Signal.TimeTravel as TimeTravel
import Data.Array (length)
import Data.Array.ST (STArray, freeze)
import Control.Monad.ST (ST, STRef, readSTRef, newSTRef, writeSTRef)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Test.Spec.Reporter.Spec (specReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Test.Signal.TimeTravel.Mock


getState'
  ∷ forall effects
  . MonadEff (st ∷ ST { action ∷ Action, state ∷ Int } | effects)
        (Aff (st ∷ ST { action ∷ Action, state ∷ Int } | effects))
  ⇒ { pointer ∷ STRef { action ∷ Action, state ∷ Int } Int
    , breadcrumb ∷ STArray { action ∷ Action, state ∷ Int } { action ∷ Action, state ∷ Int }
    }
  → STRef { action ∷ Action, state ∷ Int } Int 
  → Aff (st ∷ ST { action ∷ Action, state ∷ Int } | effects)
        { pointer ∷ Int 
        , breadcrumb ∷ Array { action ∷ Action, state ∷ Int }
        , currentState ∷ Int 
        }
getState' trail state = liftEff do
  pointer ← readSTRef trail.pointer
  breadcrumb ← freeze trail.breadcrumb
  currentState ← readSTRef state 
  pure { pointer, breadcrumb, currentState }


main ∷ Eff (RunnerEffects (channel ∷ CHANNEL, st ∷ ST { action ∷ Action, state ∷ Int })) Unit
main = do
  channel ← Channel.channel Noop
  let signal = Channel.subscribe channel
  timeTravel ← TimeTravel.initialize channel update
  trail ← timeTravel.read
  let stateSignal = foldp timeTravel.update initialState signal

  -- | Capture the current state of the signal
  currentState' ← newSTRef initialState
  runSignal $ stateSignal ~> \state → pure unit <* writeSTRef currentState' state

  let getState = getState' trail currentState'

  run [specReporter] do
    describe "Signal.TimeTravel" do
      describe "update" do
        it "should capture the initial action" do
          { pointer, breadcrumb, currentState } ← getState 

          pointer `shouldEqual` ((length breadcrumb) - 1)
          (Crumb <$> breadcrumb) `shouldEqual` [Crumb { action: Noop, state: 0 }]
          pointer `shouldEqual` 0
          currentState `shouldEqual` initialState


      describe "send action" do
        it "should capture the sent action" do
          liftEff $ Channel.send channel Increment
          { pointer, breadcrumb, currentState } ← getState 

          length breadcrumb `shouldEqual` 2
          (Crumb <$> breadcrumb) `shouldEqual` [ Crumb { action: Noop,      state: 0 }
                                               , Crumb { action: Increment, state: 1 }
                                               ]
          pointer `shouldEqual` ((length breadcrumb) - 1)
          currentState `shouldEqual` 1 


        it "should capture the next send action" do
          liftEff $ Channel.send channel Increment
          { pointer, breadcrumb, currentState } ← getState 

          length breadcrumb `shouldEqual` 3
          (Crumb <$> breadcrumb) `shouldEqual` [ Crumb { action: Noop,      state: 0 }
                                               , Crumb { action: Increment, state: 1 }
                                               , Crumb { action: Increment, state: 2 }
                                               ]
          pointer `shouldEqual` ((length breadcrumb) - 1)
          currentState `shouldEqual` 2 


      describe "prev" do
        it "should move the pointer to the previous action" do
          { currentState: oldState } ← getState
          liftEff $ timeTravel.prev
          { pointer, breadcrumb, currentState } ← getState 

          length breadcrumb `shouldEqual` 3
          (Crumb <$> breadcrumb) `shouldEqual` [ Crumb { action: Noop,      state: 0 }
                                               , Crumb { action: Increment, state: 1 }
                                               , Crumb { action: Increment, state: 2 }
                                               ]
          pointer `shouldEqual` ((length breadcrumb) - 1 - 1)
          oldState `shouldNotEqual` currentState
          currentState `shouldEqual` 1


        it "should move the pointer again to the initial action" do
          { currentState: oldState } ← getState
          liftEff $ timeTravel.prev
          { pointer, breadcrumb, currentState } ← getState 

          length breadcrumb `shouldEqual` 3
          (Crumb <$> breadcrumb) `shouldEqual` [ Crumb { action: Noop,      state: 0 }
                                               , Crumb { action: Increment, state: 1 }
                                               , Crumb { action: Increment, state: 2 }
                                               ]
          pointer `shouldEqual` 0
          oldState `shouldNotEqual` currentState
          currentState `shouldEqual` 0


        it "should not move past the initial action" do
          { currentState: oldState } ← getState
          liftEff $ timeTravel.prev
          { pointer, breadcrumb, currentState } ← getState 

          length breadcrumb `shouldEqual` 3
          (Crumb <$> breadcrumb) `shouldEqual` [ Crumb { action: Noop,      state: 0 }
                                               , Crumb { action: Increment, state: 1 }
                                               , Crumb { action: Increment, state: 2 }
                                               ]
          pointer `shouldEqual` 0
          oldState `shouldEqual` currentState
          currentState `shouldEqual` 0


      describe "next" do
        it "should move the pointer to the next action" do
          { currentState: oldState } ← getState
          liftEff $ timeTravel.next
          { pointer, breadcrumb, currentState } ← getState 

          length breadcrumb `shouldEqual` 3
          (Crumb <$> breadcrumb) `shouldEqual` [ Crumb { action: Noop,      state: 0 }
                                               , Crumb { action: Increment, state: 1 }
                                               , Crumb { action: Increment, state: 2 }
                                               ]
          pointer `shouldEqual` 1
          oldState `shouldNotEqual` currentState
          currentState `shouldEqual` 1


      describe "sending an action" do
        it "should move the pointer to the end of the breadcrumb if an action is sent" do
          { currentState: oldState } ← getState
          liftEff $ Channel.send channel Decrement
          { pointer, breadcrumb, currentState } ← getState 

          length breadcrumb `shouldEqual` 4
          (Crumb <$> breadcrumb) `shouldEqual` [ Crumb { action: Noop,      state: 0 }
                                               , Crumb { action: Increment, state: 1 }
                                               , Crumb { action: Increment, state: 2 }
                                               , Crumb { action: Decrement, state: 0 }
                                               ]
          pointer `shouldEqual` 3
          oldState `shouldNotEqual` currentState
          currentState `shouldEqual` 0


      describe "next" do
        it "should not move the pointer past the breadcrumb" do 
          { pointer: oldPointer, breadcrumb } ← getState 
          oldPointer `shouldEqual` ((length breadcrumb) - 1)

          liftEff $ timeTravel.next

          { pointer } ← getState
          pointer `shouldEqual` ((length breadcrumb) - 1)
