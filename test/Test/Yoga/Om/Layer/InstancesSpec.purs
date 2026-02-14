module Test.Yoga.Om.Layer.InstancesSpec where

import Prelude

import Control.Parallel (parApply)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer, runLayer)

spec :: Spec Unit
spec = do

  describe "Functor / Apply / Bind" do

    it "maps over layer output" do
      let
        layer :: OmLayer () () { value :: Int }
        layer = makeLayer (pure { value: 10 })

        mapped = layer <#> \r -> { doubled: r.value * 2 }

      result <- Om.runOm {} { exception: \_ -> pure { doubled: 0 } } (runLayer {} mapped)
      result.doubled `shouldEqual` 20

    it "applies two layers" do
      let
        fLayer :: OmLayer () () ({ a :: Int } -> { a :: Int, b :: String })
        fLayer = makeLayer (pure \r -> { a: r.a, b: show r.a })

        aLayer :: OmLayer () () { a :: Int }
        aLayer = makeLayer (pure { a: 42 })

      result <- Om.runOm {} { exception: \_ -> pure { a: 0, b: "" } } (runLayer {} (fLayer <*> aLayer))
      result.a `shouldEqual` 42
      result.b `shouldEqual` "42"

    it "binds layers sequentially" do
      let
        layer = do
          r <- makeLayer (pure { x: 5 }) :: OmLayer () () { x :: Int }
          makeLayer (pure { result: r.x + 10 })

      result <- Om.runOm {} { exception: \_ -> pure { result: 0 } } (runLayer {} layer)
      result.result `shouldEqual` 15

  describe "Parallel" do

    it "runs layers in parallel via parApply" do
      log <- liftEffect $ Ref.new []
      let
        layerA :: OmLayer () () { a :: String }
        layerA = makeLayer do
          liftEffect $ Ref.modify_ (_ <> [ "a" ]) log
          pure { a: "A" }

        layerB :: OmLayer () () { b :: String }
        layerB = makeLayer do
          liftEffect $ Ref.modify_ (_ <> [ "b" ]) log
          pure { b: "B" }

        combined = parApply (map (\ra rb -> { a: ra.a, b: rb.b }) layerA) layerB

      result <- Om.runOm {} { exception: \_ -> pure { a: "", b: "" } } (runLayer {} combined)
      result.a `shouldEqual` "A"
      result.b `shouldEqual` "B"
      finalLog <- liftEffect $ Ref.read log
      finalLog `shouldEqual` [ "a", "b" ]
