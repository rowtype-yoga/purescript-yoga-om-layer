module Test.Playground where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as Console
import Type.Row (type (+))
import Yoga.Om (launchOm_)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer, runLayer)

-- Define some example types
type Config = { port :: Int, host :: String }
type ConfigL r = (config :: Config | r)
type ConfigL_ = ConfigL ()
type Logger = { log :: String -> Effect Unit }
type LoggerL r = (logger :: Logger | r)
type LoggerL_ = LoggerL ()
type Database = { query :: String -> Effect (Array String) }
type DatabaseL r = (database :: Database | r)
type DatabaseL_ = DatabaseL ()
type Cache = { get :: String -> Effect (Maybe String) }
type CacheL r = (cache :: Cache | r)
type CacheL_ = CacheL ()

loggerLayer :: OmLayer ConfigL_ () { logger :: Logger }
loggerLayer = makeLayer do
  { config } <- Om.ask
  pure { logger: { log: \msg -> pure unit } }

databaseLayer :: OmLayer ConfigL_ () { database :: Database }
databaseLayer = makeLayer do
  { config } <- Om.ask
  pure { database: { query: \q -> pure [] } }

cacheLayer :: OmLayer (LoggerL + DatabaseL_) () { cache :: Cache }
cacheLayer = makeLayer do
  { logger, database } <- Om.ask
  pure { cache: { get: \key -> pure Nothing } }

programLayer :: OmLayer (DatabaseL + CacheL_) () {}
programLayer = makeLayer do
  pure {}

aProgram = launchOm_ do
  programLayer # runLayer
    { config: { port: 5432, host: "localhost" }
    , logger: { log: Console.log }
    , database: { query: \q -> pure [] }
    , cache: { get: \key -> pure Nothing }
    }
