module Test.Yoga.Om.Layer.PureInferenceSpec where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, withValue, makeLayer, runLayer)

spec :: Spec Unit
spec = describe "withValue" do

  it "combines a plain record with a layer without needing VTA" do
    let
      layer :: OmLayer () () { value :: Int }
      layer = makeLayer (pure { value: 42 })

      result = layer # withValue { extra: "hello" }

    out <- Om.runOm {} { exception: \_ -> pure { value: -1, extra: "" } } (runLayer {} result)
    out.value `shouldEqual` 42
    out.extra `shouldEqual` "hello"
