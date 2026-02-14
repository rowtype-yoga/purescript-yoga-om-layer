-- EXPECT: Missing required dependency
module Yoga.Om.Layer.CompileFailTest where

import Prelude
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer, runLayer)

layer :: OmLayer (a :: Int, b :: String) () { out :: String }
layer = makeLayer do
  { a, b } <- Om.ask
  pure { out: show a <> b }

bad = runLayer { a: 42 } layer
