module Test.Yoga.Om.Layer.ScopedSpec where

import Prelude

import Effect.Aff (throwError, try)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, Scope, makeLayer, makeScopedLayer, bracketLayer, fresh, combineRequirements, runScoped, withScoped, provide)

spec :: Spec Unit
spec = describe "Scoped Layers" do

  it "runs finalizers in reverse order on success" do
    log <- liftEffect $ Ref.new []
    let
      layerA :: OmLayer (scope :: Scope) () { a :: String }
      layerA = makeScopedLayer
        (pure { a: "A" })
        (\_ -> liftEffect $ Ref.modify_ (_ <> [ "release-A" ]) log)

      layerB :: OmLayer (scope :: Scope) () { b :: String }
      layerB = makeScopedLayer
        (pure { b: "B" })
        (\_ -> liftEffect $ Ref.modify_ (_ <> [ "release-B" ]) log)

      layerC :: OmLayer (scope :: Scope) () { c :: String }
      layerC = makeScopedLayer
        (pure { c: "C" })
        (\_ -> liftEffect $ Ref.modify_ (_ <> [ "release-C" ]) log)

      layerBC :: OmLayer (scope :: Scope) () { b :: String, c :: String }
      layerBC = combineRequirements layerB layerC

      combined = combineRequirements layerA layerBC

    _ <- liftAff $ runScoped combined
    finalLog <- liftEffect $ Ref.read log
    finalLog `shouldEqual` [ "release-C", "release-B", "release-A" ]

  it "runs finalizers when the callback throws" do
    log <- liftEffect $ Ref.new []
    let
      layer :: OmLayer (scope :: Scope) () { value :: String }
      layer = makeScopedLayer
        (pure { value: "acquired" })
        (\_ -> liftEffect $ Ref.modify_ (_ <> [ "released" ]) log)

    _ <- liftAff $ try $ withScoped layer \_ ->
      throwError (error "callback failed")
    finalLog <- liftEffect $ Ref.read log
    finalLog `shouldEqual` [ "released" ]

  it "threads finalizers through vertical composition (provide)" do
    log <- liftEffect $ Ref.new []
    let
      baseLayer :: OmLayer (scope :: Scope) () { base :: String }
      baseLayer = makeScopedLayer
        (pure { base: "base-value" })
        (\_ -> liftEffect $ Ref.modify_ (_ <> [ "release-base" ]) log)

      upperLayer :: OmLayer (scope :: Scope, base :: String) () { upper :: String }
      upperLayer = makeScopedLayer
        ( do
            { base } <- Om.ask
            pure { upper: base <> "-extended" }
        )
        (\_ -> liftEffect $ Ref.modify_ (_ <> [ "release-upper" ]) log)

      composed = upperLayer `provide` baseLayer

    result <- liftAff $ runScoped composed
    result.upper `shouldEqual` "base-value-extended"
    finalLog <- liftEffect $ Ref.read log
    finalLog `shouldEqual` [ "release-upper", "release-base" ]

  it "works with bracketLayer" do
    log <- liftEffect $ Ref.new []
    let
      layer = bracketLayer
        ( do
            liftEffect $ Ref.modify_ (_ <> [ "acquire" ]) log
            pure 42
        )
        (\_ -> liftEffect $ Ref.modify_ (_ <> [ "release" ]) log)
        (\n -> pure { value: n })

    result <- liftAff $ runScoped layer
    result.value `shouldEqual` 42
    finalLog <- liftEffect $ Ref.read log
    finalLog `shouldEqual` [ "acquire", "release" ]

  it "fresh layer builds twice even when same base layer" do
    counter <- liftEffect $ Ref.new 0
    let
      baseLayer :: OmLayer (scope :: Scope) () { value :: String }
      baseLayer = makeLayer do
        liftEffect $ Ref.modify_ (_ + 1) counter
        pure { value: "built" }

      branch1 :: OmLayer (scope :: Scope) () { out1 :: String }
      branch1 = consumer `provide` fresh baseLayer
        where
        consumer :: OmLayer (scope :: Scope, value :: String) () { out1 :: String }
        consumer = makeLayer do
          { value } <- Om.ask
          pure { out1: value <> "-1" }

      branch2 :: OmLayer (scope :: Scope) () { out2 :: String }
      branch2 = consumer `provide` fresh baseLayer
        where
        consumer :: OmLayer (scope :: Scope, value :: String) () { out2 :: String }
        consumer = makeLayer do
          { value } <- Om.ask
          pure { out2: value <> "-2" }

      app = combineRequirements branch1 branch2

    result <- liftAff $ runScoped app
    result.out1 `shouldEqual` "built-1"
    result.out2 `shouldEqual` "built-2"
    count <- liftEffect $ Ref.read counter
    count `shouldEqual` 2

  it "combines scoped and non-scoped layers" do
    log <- liftEffect $ Ref.new []
    let
      scopedLayer :: OmLayer (scope :: Scope) () { scoped :: String }
      scopedLayer = makeScopedLayer
        (pure { scoped: "yes" })
        (\_ -> liftEffect $ Ref.modify_ (_ <> [ "release-scoped" ]) log)

      plainLayer :: OmLayer () () { plain :: String }
      plainLayer = makeLayer (pure { plain: "no-finalizer" })

      combined = combineRequirements scopedLayer plainLayer

    result <- liftAff $ runScoped combined
    result.scoped `shouldEqual` "yes"
    result.plain `shouldEqual` "no-finalizer"
    finalLog <- liftEffect $ Ref.read log
    finalLog `shouldEqual` [ "release-scoped" ]
