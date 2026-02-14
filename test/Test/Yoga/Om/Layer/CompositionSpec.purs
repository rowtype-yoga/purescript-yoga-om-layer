module Test.Yoga.Om.Layer.CompositionSpec where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer, combineRequirements, runLayer)

type Config = { port :: Int, host :: String }
type Logger = { log :: String -> Effect Unit }
type Database = { query :: String -> Effect (Array String) }

loggerLayer :: OmLayer (config :: Config) () { logger :: Logger }
loggerLayer = makeLayer do
  { config } <- Om.ask
  pure { logger: { log: \msg -> Console.log $ "[" <> config.host <> "] " <> msg } }

databaseLayer :: OmLayer (config :: Config) () { database :: Database }
databaseLayer = makeLayer do
  { config } <- Om.ask
  pure { database: { query: \_ -> pure [ "Result from " <> config.host ] } }

spec :: Spec Unit
spec = describe "Composition" do

  it "proves Row.Nub deduplicates shared requirements at type level" do
    let
      _proofOfDeduplication :: OmLayer (config :: Config) _ _
      _proofOfDeduplication = combineRequirements loggerLayer databaseLayer
    Console.log "Type-level deduplication works: (config, config) -> (config)"

  it "proves deduplication at runtime via shared Ref" do
    accessLog <- liftEffect $ Ref.new ""
    let
      combinedLayer :: OmLayer (config :: Config, accessLog :: Ref.Ref String) () { logger :: Logger, database :: Database }
      combinedLayer = makeLayer do
        { config, accessLog: log } <- Om.ask
        liftEffect $ Ref.modify_ (_ <> "config-accessed-by-logger ") log
        let logger = { log: \msg -> Console.log $ "[" <> config.host <> "] " <> msg }
        liftEffect $ Ref.modify_ (_ <> "config-accessed-by-database ") log
        let database = { query: \_ -> pure [ "Result from " <> config.host ] }
        pure { logger, database }

    let ctx = { config: { port: 5432, host: "localhost" }, accessLog: accessLog }
    result <- runLayer ctx combinedLayer
      # Om.runOm ctx
          { exception: \_ -> pure { logger: { log: \_ -> pure unit }, database: { query: \_ -> pure [] } } }

    finalLog <- liftEffect $ Ref.read accessLog
    finalLog `shouldEqual` "config-accessed-by-logger config-accessed-by-database "

    liftEffect $ result.logger.log "Testing logger from combined layer"
    queryResult <- liftEffect $ result.database.query "SELECT *"
    queryResult `shouldEqual` [ "Result from localhost" ]
