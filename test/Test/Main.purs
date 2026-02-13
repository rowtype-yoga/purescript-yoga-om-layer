module Test.Main where

import Prelude

import Control.Parallel (parApply)
import Effect (Effect)
import Effect.Aff (launchAff_, throwError, try)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Data.Either (Either(..))
import Data.Variant (match)
import Yoga.Om as Om
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff.Retry (RetryStatus(..), limitRetries, constantDelay)
import Yoga.Om.Layer (OmLayer, Scope, makeLayer, makeScopedLayer, bracketLayer, fresh, combineRequirements, runLayer, runScoped, runScopedWith, withScoped, provide, recovering, repeating)

-- Example types
type Config = { port :: Int, host :: String }
type Logger = { log :: String -> Effect Unit }
type Database = { query :: String -> Effect (Array String) }

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "OmLayer - Proof of Concept: Type-Level Deduplication" do

    it "proves Row.Nub deduplicates shared requirements at type level" do
      let
        loggerLayer :: OmLayer (config :: Config) () { logger :: Logger }
        loggerLayer = makeLayer do
          { config } <- Om.ask
          pure { logger: { log: \msg -> Console.log $ "[" <> config.host <> "] " <> msg } }

        databaseLayer :: OmLayer (config :: Config) () { database :: Database }
        databaseLayer = makeLayer do
          { config } <- Om.ask
          pure { database: { query: \q -> pure [ "Result from " <> config.host ] } }

      let
        _proofOfDeduplication :: OmLayer (config :: Config) _ _
        _proofOfDeduplication = combineRequirements loggerLayer databaseLayer

      Console.log "Type-level deduplication works: (config, config) -> (config)"
      pure unit

    it "proves deduplication at runtime via shared Ref" do
      accessLog <- liftEffect $ Ref.new ""
      let
        combinedLayer :: OmLayer (config :: Config, accessLog :: Ref.Ref String) () { logger :: Logger, database :: Database }
        combinedLayer = makeLayer do
          { config, accessLog: log } <- Om.ask
          liftEffect $ Ref.modify_ (_ <> "config-accessed-by-logger ") log
          let logger = { log: \msg -> Console.log $ "[" <> config.host <> "] " <> msg }
          liftEffect $ Ref.modify_ (_ <> "config-accessed-by-database ") log
          let database = { query: \q -> pure [ "Result from " <> config.host ] }
          pure { logger, database }

      let
        ctx =
          { config: { port: 5432, host: "localhost" }
          , accessLog: accessLog
          }
      result <- runLayer ctx combinedLayer
        # Om.runOm ctx
            { exception: \_ -> pure { logger: { log: \_ -> pure unit }, database: { query: \_ -> pure [] } } }

      finalLog <- liftEffect $ Ref.read accessLog
      finalLog `shouldEqual` "config-accessed-by-logger config-accessed-by-database "

      liftEffect $ result.logger.log "Testing logger from combined layer"
      queryResult <- liftEffect $ result.database.query "SELECT *"
      queryResult `shouldEqual` [ "Result from localhost" ]

  describe "Scoped Layers" do

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

  describe "Automatic Memoization" do

    it "builds the same layer only once when used in two branches" do
      counter <- liftEffect $ Ref.new 0
      let
        expensiveLayer :: OmLayer (scope :: Scope) () { value :: String }
        expensiveLayer = makeLayer do
          liftEffect $ Ref.modify_ (_ + 1) counter
          pure { value: "built" }

        branch1 :: OmLayer (scope :: Scope) () { out1 :: String }
        branch1 = consumer `provide` expensiveLayer
          where
          consumer :: OmLayer (scope :: Scope, value :: String) () { out1 :: String }
          consumer = makeLayer do
            { value } <- Om.ask
            pure { out1: value <> "-1" }

        branch2 :: OmLayer (scope :: Scope) () { out2 :: String }
        branch2 = consumer `provide` expensiveLayer
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
      count `shouldEqual` 1

    it "memoized scoped layer runs finalizer only once" do
      log <- liftEffect $ Ref.new []
      let
        dbLayer :: OmLayer (scope :: Scope) () { db :: String }
        dbLayer = makeScopedLayer
          (pure { db: "connected" })
          (\_ -> liftEffect $ Ref.modify_ (_ <> [ "db-closed" ]) log)

        repoBranch :: OmLayer (scope :: Scope) () { repo :: String }
        repoBranch = repo `provide` dbLayer
          where
          repo :: OmLayer (scope :: Scope, db :: String) () { repo :: String }
          repo = makeLayer do
            { db } <- Om.ask
            pure { repo: db <> "-repo" }

        analyticsBranch :: OmLayer (scope :: Scope) () { analytics :: String }
        analyticsBranch = analytics `provide` dbLayer
          where
          analytics :: OmLayer (scope :: Scope, db :: String) () { analytics :: String }
          analytics = makeLayer do
            { db } <- Om.ask
            pure { analytics: db <> "-analytics" }

        app = combineRequirements repoBranch analyticsBranch

      result <- liftAff $ runScoped app
      result.repo `shouldEqual` "connected-repo"
      result.analytics `shouldEqual` "connected-analytics"
      finalLog <- liftEffect $ Ref.read log
      finalLog `shouldEqual` [ "db-closed" ]

    it "memoized bracketLayer acquires and releases only once" do
      log <- liftEffect $ Ref.new []
      let
        connLayer :: OmLayer (scope :: Scope) () { conn :: Int }
        connLayer = bracketLayer
          ( do
              liftEffect $ Ref.modify_ (_ <> [ "acquire" ]) log
              pure 42
          )
          (\_ -> liftEffect $ Ref.modify_ (_ <> [ "release" ]) log)
          (\n -> pure { conn: n })

        branch1 :: OmLayer (scope :: Scope) () { x :: String }
        branch1 = upper `provide` connLayer
          where
          upper :: OmLayer (scope :: Scope, conn :: Int) () { x :: String }
          upper = makeLayer do
            { conn } <- Om.ask
            pure { x: show conn }

        branch2 :: OmLayer (scope :: Scope) () { y :: String }
        branch2 = upper `provide` connLayer
          where
          upper :: OmLayer (scope :: Scope, conn :: Int) () { y :: String }
          upper = makeLayer do
            { conn } <- Om.ask
            pure { y: show (conn + 1) }

        app = combineRequirements branch1 branch2

      result <- liftAff $ runScoped app
      result.x `shouldEqual` "42"
      result.y `shouldEqual` "43"
      finalLog <- liftEffect $ Ref.read log
      finalLog `shouldEqual` [ "acquire", "release" ]

  describe "Error Channel" do

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

  describe "Layer recovering" do

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

  describe "Layer repeating" do

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
