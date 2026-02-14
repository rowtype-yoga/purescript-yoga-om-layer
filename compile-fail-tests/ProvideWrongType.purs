-- EXPECT: TypesDoNotUnify
module Yoga.Om.Layer.CompileFailTest where

import Prelude
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer, provide)

base :: OmLayer () () { value :: Int }
base = makeLayer (pure { value: 42 })

upper :: OmLayer (value :: String) () { out :: String }
upper = makeLayer do
  { value } <- Om.ask
  pure { out: value }

bad = upper `provide` base
