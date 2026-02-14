module Test.Yoga.Om.Layer.RecoveringSpec where

import Prelude

import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Data.Either (Either(..))
import Data.Variant (match)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Retry (RetryStatus(..), limitRetries, constantDelay)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer, runLayer, recovering)

spec :: Spec Unit
spec = describe "Layer recovering" do

  it "retries layer construction on matching errors" do
    attemptsRef <- liftEffect $ Ref.new 0
    let
      layer :: OmLayer () (dbError :: String) { db :: String }
      layer = makeLayer do
        attempts <- Ref.modify (_ + 1) attemptsRef # liftEffect
        when (attempts < 3) do
          Om.throw { dbError: "connection refused" }
        pure { db: "connected" }

      retried = layer
        # recovering (constantDelay (Milliseconds 0.0) <> limitRetries 5)
            \_ -> { dbError: \_ -> pure true }

    result <- Om.runOm {}
      { exception: \_ -> pure { db: "" }, dbError: \_ -> pure { db: "" } }
      (runLayer {} retried)
    result.db `shouldEqual` "connected"
    attempts <- liftEffect $ Ref.read attemptsRef
    attempts `shouldEqual` 3

  it "does not retry on non-matching errors" do
    attemptsRef <- liftEffect $ Ref.new 0
    let
      layer :: OmLayer () (dbError :: String, authError :: String) { db :: String }
      layer = makeLayer do
        Ref.modify_ (_ + 1) attemptsRef # liftEffect
        Om.throw { authError: "unauthorized" }

      retried = layer
        # recovering (constantDelay (Milliseconds 0.0) <> limitRetries 3)
            \_ -> { dbError: \_ -> pure true }

    _ <- Om.runOm {}
      { exception: \_ -> pure { db: "" }
      , dbError: \_ -> pure { db: "" }
      , authError: \_ -> pure { db: "" }
      }
      (runLayer {} retried)
    attempts <- liftEffect $ Ref.read attemptsRef
    attempts `shouldEqual` 1

  it "preserves the error when retries are exhausted" do
    let
      layer :: OmLayer () (dbError :: String) { db :: String }
      layer = makeLayer do
        Om.throw { dbError: "connection refused" }

      retried = layer
        # recovering (constantDelay (Milliseconds 0.0) <> limitRetries 2)
            \_ -> { dbError: \_ -> pure true }

    result <- Om.runReader {} (runLayer {} retried)
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

  it "only retries the specified error in a multi-error layer" do
    attemptsRef <- liftEffect $ Ref.new 0
    let
      layer :: OmLayer () (dbError :: String, cacheError :: String) { db :: String }
      layer = makeLayer do
        attempts <- Ref.modify (_ + 1) attemptsRef # liftEffect
        if attempts <= 2 then Om.throw { dbError: "db down" }
        else Om.throw { cacheError: "cache miss" }

      retried = layer
        # recovering (constantDelay (Milliseconds 0.0) <> limitRetries 5)
            \_ -> { dbError: \_ -> pure true }

    result <- Om.runReader {} (runLayer {} retried)
      # liftAff
    case result of
      Left err -> do
        let
          msg = err # match
            { exception: \_ -> "exception"
            , dbError: \e -> e
            , cacheError: \e -> e
            }
        msg `shouldEqual` "cache miss"
      Right _ -> "should have failed" `shouldEqual` "but succeeded"
    attempts <- liftEffect $ Ref.read attemptsRef
    attempts `shouldEqual` 3

  it "uses RetryStatus in the check" do
    attemptsRef <- liftEffect $ Ref.new 0
    let
      layer :: OmLayer () (dbError :: String) { db :: String }
      layer = makeLayer do
        Ref.modify_ (_ + 1) attemptsRef # liftEffect
        Om.throw { dbError: "fail" }

      retried = layer
        # recovering (constantDelay (Milliseconds 0.0) <> limitRetries 10)
            \(RetryStatus s) -> { dbError: \_ -> pure (s.iterNumber < 2) }

    _ <- Om.runOm {}
      { exception: \_ -> pure { db: "" }, dbError: \_ -> pure { db: "" } }
      (runLayer {} retried)
    attempts <- liftEffect $ Ref.read attemptsRef
    attempts `shouldEqual` 3
