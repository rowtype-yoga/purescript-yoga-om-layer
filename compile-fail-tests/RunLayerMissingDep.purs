-- EXPECT: Missing required dependency
module Yoga.Om.Layer.CompileFailTest where

import Prelude
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer, runLayer)

type Database = { query :: String -> Array String }

dbLayer :: OmLayer (config :: { host :: String }) () { db :: Database }
dbLayer = makeLayer do
  { config } <- Om.ask
  pure { db: { query: \_ -> [ config.host ] } }

bad = runLayer {} dbLayer
