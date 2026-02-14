module Test.Yoga.Om.Layer.ErrorChannelSpec where

import Prelude

import Effect.Aff (try)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Data.Either (Either(..))
import Data.Variant (match)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, Scope, makeLayer, makeScopedLayer, combineRequirements, runLayer, runScopedWith, provide)

spec :: Spec Unit
spec = describe "Error Channel" do

  it "composes layers with different error types" do
    let
      dbLayer :: OmLayer () (dbError :: String) { db :: String }
      dbLayer = makeLayer do
        pure { db: "connected" }

      cacheLayer :: OmLayer () (cacheError :: String) { cache :: String }
      cacheLayer = makeLayer do
        pure { cache: "ready" }

      combined :: OmLayer () (dbError :: String, cacheError :: String) { db :: String, cache :: String }
      combined = combineRequirements dbLayer cacheLayer

    result <- Om.runOm {}
      { exception: \_ -> pure { db: "", cache: "" }
      , dbError: \_ -> pure { db: "", cache: "" }
      , cacheError: \_ -> pure { db: "", cache: "" }
      }
      (runLayer {} combined)
    result.db `shouldEqual` "connected"
    result.cache `shouldEqual` "ready"

  it "propagates errors from the first layer" do
    let
      failingLayer :: OmLayer () (dbError :: String) { db :: String }
      failingLayer = makeLayer do
        Om.throw { dbError: "connection refused" }

      okLayer :: OmLayer () () { cache :: String }
      okLayer = makeLayer do
        pure { cache: "ready" }

      combined = combineRequirements failingLayer okLayer

    result <- Om.runReader {} (runLayer {} combined)
      # liftAff
    case result of
      Left err -> do
        let
          msg = err # match
            { exception: \_ -> "exception"
            , dbError: \e -> e
            }
        msg `shouldEqual` "connection refused"
      Right _ -> "should have failed" `shouldEqual` "but succeeded"

  it "propagates errors from the second layer" do
    let
      okLayer :: OmLayer () () { db :: String }
      okLayer = makeLayer do
        pure { db: "connected" }

      failingLayer :: OmLayer () (cacheError :: String) { cache :: String }
      failingLayer = makeLayer do
        Om.throw { cacheError: "cache unavailable" }

      combined = combineRequirements okLayer failingLayer

    result <- Om.runReader {} (runLayer {} combined)
      # liftAff
    case result of
      Left err -> do
        let
          msg = err # match
            { exception: \_ -> "exception"
            , cacheError: \e -> e
            }
        msg `shouldEqual` "cache unavailable"
      Right _ -> "should have failed" `shouldEqual` "but succeeded"

  it "propagates errors through vertical composition" do
    let
      baseLayer :: OmLayer () (baseError :: String) { base :: String }
      baseLayer = makeLayer do
        Om.throw { baseError: "base failed" }

      upperLayer :: OmLayer (base :: String) (upperError :: String) { upper :: String }
      upperLayer = makeLayer do
        { base } <- Om.ask
        pure { upper: base <> "-extended" }

      composed = upperLayer `provide` baseLayer

    result <- Om.runReader {} (runLayer {} composed)
      # liftAff
    case result of
      Left err -> do
        let
          msg = err # match
            { exception: \_ -> "exception"
            , baseError: \e -> e
            , upperError: \_ -> "upper"
            }
        msg `shouldEqual` "base failed"
      Right _ -> "should have failed" `shouldEqual` "but succeeded"

  it "scoped layers with errors run via runScopedWith" do
    log <- liftEffect $ Ref.new []
    let
      dbLayer :: OmLayer (scope :: Scope) (dbError :: String) { db :: String }
      dbLayer = makeScopedLayer
        (pure { db: "connected" })
        (\_ -> liftEffect $ Ref.modify_ (_ <> [ "db-closed" ]) log)

      cacheLayer :: OmLayer (scope :: Scope) (cacheError :: String) { cache :: String }
      cacheLayer = makeScopedLayer
        (pure { cache: "ready" })
        (\_ -> liftEffect $ Ref.modify_ (_ <> [ "cache-closed" ]) log)

      combined = combineRequirements dbLayer cacheLayer

    let
      handlers =
        { exception: \_ -> pure { db: "", cache: "" }
        , dbError: \_ -> pure { db: "", cache: "" }
        , cacheError: \_ -> pure { db: "", cache: "" }
        }
    result <- liftAff $ runScopedWith handlers combined
    result.db `shouldEqual` "connected"
    result.cache `shouldEqual` "ready"
    finalLog <- liftEffect $ Ref.read log
    finalLog `shouldEqual` [ "cache-closed", "db-closed" ]

  it "scoped layer error triggers finalizers" do
    log <- liftEffect $ Ref.new []
    let
      goodLayer :: OmLayer (scope :: Scope) () { good :: String }
      goodLayer = makeScopedLayer
        (pure { good: "ok" })
        (\_ -> liftEffect $ Ref.modify_ (_ <> [ "good-closed" ]) log)

      failLayer :: OmLayer (scope :: Scope) (buildError :: String) { bad :: String }
      failLayer = makeScopedLayer
        (Om.throw { buildError: "build failed" })
        (\_ -> liftEffect $ Ref.modify_ (_ <> [ "bad-closed" ]) log)

      combined = combineRequirements goodLayer failLayer

    let
      handlers =
        { exception: \_ -> pure { good: "", bad: "" }
        , buildError: \_ -> pure { good: "", bad: "" }
        }
    result <- liftAff $ try $ runScopedWith handlers combined
    case result of
      Right r -> r.good `shouldEqual` ""
      Left _ -> pure unit
    finalLog <- liftEffect $ Ref.read log
    finalLog `shouldEqual` [ "good-closed" ]
