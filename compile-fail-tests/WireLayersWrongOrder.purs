-- EXPECT: TypesDoNotUnify
module Yoga.Om.Layer.CompileFailTest where

import Prelude
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, Scope, makeLayer, wireLayers)

-- a_base needs upper, but b_upper provides it — wrong alphabetical order
a_needs_upper :: OmLayer (scope :: Scope, upper :: String) () { final :: String }
a_needs_upper = makeLayer do
  { upper } <- Om.ask
  pure { final: upper <> "!" }

b_upper :: OmLayer (scope :: Scope) () { upper :: String }
b_upper = makeLayer (pure { upper: "hello" })

bad = wireLayers { a_needs_upper, b_upper }
