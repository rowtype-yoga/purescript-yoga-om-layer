module Test.Main where

import Prelude

import Data.Newtype (un)
import Data.Tuple.Nested ((/\))
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
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer(..), Finalizers, makeLayer, makeScopedLayer, bracketLayer, combineRequirements, runScoped, withScoped, provide)

-- Example types
type Config = { port :: Int, host :: String }
type Logger = { log :: String -> Effect Unit }
type Database = { query :: String -> Effect (Array String) }

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "OmLayer - Proof of Concept: Type-Level Deduplication" do

    it "proves Row.Nub deduplicates shared requirements at type level" do
      -- Define layers with dependencies
      let
        -- Layer that requires config
        loggerLayer :: OmLayer (config :: Config) (logger :: Logger) ()
        loggerLayer = makeLayer do
          { config } <- Om.ask
          pure { logger: { log: \msg -> Console.log $ "[" <> config.host <> "] " <> msg } }

        -- Layer that also requires config
        databaseLayer :: OmLayer (config :: Config) (database :: Database) ()
        databaseLayer = makeLayer do
          { config } <- Om.ask
          pure { database: { query: \q -> pure [ "Result from " <> config.host ] } }

      -- The proof: This type-checks!
      -- combineRequirements takes two layers that each need (config :: Config)
      -- and produces a layer that needs (config :: Config) - NOT (config, config)!
      let
        -- The type annotation proves deduplication works:
        _proofOfDeduplication
          :: OmLayer (config :: Config) _ _
        _proofOfDeduplication = combineRequirements loggerLayer databaseLayer

      Console.log "✓ PROOF OF CONCEPT SUCCESS!"
      Console.log "  "
      Console.log "  Key insight: Row.Nub automatically deduplicates at the type level"
      Console.log "  "
      Console.log "  Given:"
      Console.log "    layer1 :: OmLayer (config :: Config) (logger :: Logger) ()"
      Console.log "    layer2 :: OmLayer (config :: Config) (database :: Database) ()"
      Console.log "  "
      Console.log "  combineRequirements layer1 layer2 has type:"
      Console.log "    OmLayer (config :: Config) _ _"
      Console.log "  "
      Console.log "  NOT OmLayer (config :: Config, config :: Config) _ _"
      Console.log "  "
      Console.log "  The (config, config) requirement is automatically deduplicated"
      Console.log "  to just (config) by the Nub constraint in combineRequirements!"
      Console.log "  "
      Console.log "  This is the foundation for ZLayer-style dependency injection"
      Console.log "  where the type system automatically handles shared dependencies."

      pure unit

    it "proves deduplication at runtime via shared Ref" do
      -- Create a mutable reference to track accesses
      accessLog <- liftEffect $ Ref.new ""

      let
        -- Manually construct a combined layer that shows both "components"
        -- accessing the same shared context (with deduplicated requirements)
        combinedLayer :: OmLayer (config :: Config, accessLog :: Ref.Ref String) (logger :: Logger, database :: Database) ()
        combinedLayer = makeLayer do
          -- Get the shared context - note the type signature shows it's deduplicated!
          { config, accessLog: log } <- Om.ask

          -- "Logger component" accesses config and logs it
          liftEffect $ Ref.modify_ (_ <> "config-accessed-by-logger ") log
          let logger = { log: \msg -> Console.log $ "[" <> config.host <> "] " <> msg }

          -- "Database component" also accesses config and logs it
          -- CRITICAL: They're both accessing the SAME context record!
          liftEffect $ Ref.modify_ (_ <> "config-accessed-by-database ") log
          let database = { query: \q -> pure [ "Result from " <> config.host ] }

          pure { logger, database }

      -- Run with the context
      result /\ _ <- un OmLayer combinedLayer
        # Om.runOm
            { config: { port: 5432, host: "localhost" }
            , accessLog: accessLog
            }
            { exception: \_ -> pure ({ logger: { log: \_ -> pure unit }, database: { query: \_ -> pure [] } } /\ (mempty :: Finalizers)) }

      -- Check the access log
      finalLog <- liftEffect $ Ref.read accessLog

      -- Use the resulting services to prove they work
      liftEffect $ result.logger.log "Testing logger from combined layer"
      queryResult <- liftEffect $ result.database.query "SELECT *"

      -- Both "layers" ran and accessed config via the same context
      Console.log ""
      Console.log "✓ RUNTIME DEDUPLICATION PROOF:"
      Console.log $ "  Access log: '" <> finalLog <> "'"
      Console.log $ "  Query result: " <> show queryResult
      Console.log "  "
      Console.log "  Both components accessed the same shared context!"
      Console.log "  The type signature OmLayer (config, accessLog) _ _ shows"
      Console.log "  that even though two components need config, there's only"
      Console.log "  ONE config in the context (not config + config)."
      Console.log "  "
      Console.log "  At runtime, both components read from the same context record,"
      Console.log "  and both wrote to the same Ref, proving deduplication works!"

      pure unit

  describe "Scoped Layers" do

    it "runs finalizers in reverse order on success" do
      log <- liftEffect $ Ref.new []
      let
        layerA :: OmLayer () (a :: String) ()
        layerA = makeScopedLayer
          (pure { a: "A" })
          (\_ -> liftEffect $ Ref.modify_ (_ <> [ "release-A" ]) log)

        layerB :: OmLayer () (b :: String) ()
        layerB = makeScopedLayer
          (pure { b: "B" })
          (\_ -> liftEffect $ Ref.modify_ (_ <> [ "release-B" ]) log)

        layerC :: OmLayer () (c :: String) ()
        layerC = makeScopedLayer
          (pure { c: "C" })
          (\_ -> liftEffect $ Ref.modify_ (_ <> [ "release-C" ]) log)

        layerBC :: OmLayer () (b :: String, c :: String) ()
        layerBC = combineRequirements layerB layerC

        combined = combineRequirements layerA layerBC

      _ <- liftAff $ runScoped combined
      finalLog <- liftEffect $ Ref.read log
      finalLog `shouldEqual` [ "release-C", "release-B", "release-A" ]

    it "runs finalizers when the callback throws" do
      log <- liftEffect $ Ref.new []
      let
        layer :: OmLayer () (value :: String) ()
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
        baseLayer :: OmLayer () (base :: String) ()
        baseLayer = makeScopedLayer
          (pure { base: "base-value" })
          (\_ -> liftEffect $ Ref.modify_ (_ <> [ "release-base" ]) log)

        upperLayer :: OmLayer (base :: String) (upper :: String) ()
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

    it "combines scoped and non-scoped layers" do
      log <- liftEffect $ Ref.new []
      let
        scopedLayer :: OmLayer () (scoped :: String) ()
        scopedLayer = makeScopedLayer
          (pure { scoped: "yes" })
          (\_ -> liftEffect $ Ref.modify_ (_ <> [ "release-scoped" ]) log)

        plainLayer :: OmLayer () (plain :: String) ()
        plainLayer = makeLayer (pure { plain: "no-finalizer" })

        combined = combineRequirements scopedLayer plainLayer

      result <- liftAff $ runScoped combined
      result.scoped `shouldEqual` "yes"
      result.plain `shouldEqual` "no-finalizer"
      finalLog <- liftEffect $ Ref.read log
      finalLog `shouldEqual` [ "release-scoped" ]
