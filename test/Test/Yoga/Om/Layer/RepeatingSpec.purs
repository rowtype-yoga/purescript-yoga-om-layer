module Test.Yoga.Om.Layer.RepeatingSpec where

import Prelude

import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Retry (limitRetries, constantDelay)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer, runLayer, repeating)

spec :: Spec Unit
spec = describe "Layer repeating" do

  it "repeats layer construction while condition holds" do
    counterRef <- liftEffect $ Ref.new 0
    let
      layer :: OmLayer () () { count :: Int }
      layer = makeLayer do
        count <- Ref.modify (_ + 1) counterRef # liftEffect
        pure { count }

      repeated = layer
        # repeating (constantDelay (Milliseconds 0.0) <> limitRetries 10)
            \_ r -> pure (r.count < 5)

    result <- Om.runOm {}
      { exception: \_ -> pure { count: 0 } }
      (runLayer {} repeated)
    result.count `shouldEqual` 5

  it "stops when policy exhausted" do
    counterRef <- liftEffect $ Ref.new 0
    let
      layer :: OmLayer () () { count :: Int }
      layer = makeLayer do
        count <- Ref.modify (_ + 1) counterRef # liftEffect
        pure { count }

      repeated = layer
        # repeating (constantDelay (Milliseconds 0.0) <> limitRetries 3)
            \_ _ -> pure true

    result <- Om.runOm {}
      { exception: \_ -> pure { count: 0 } }
      (runLayer {} repeated)
    result.count `shouldEqual` 4
