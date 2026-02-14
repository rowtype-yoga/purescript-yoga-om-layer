-- EXPECT: TypesDoNotUnify
module Yoga.Om.Layer.CompileFailTest where

import Prelude
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, Scope, makeLayer, wireLayers)

a_base :: OmLayer (scope :: Scope) () { base :: String }
a_base = makeLayer (pure { base: "ok" })

b_upper :: OmLayer (scope :: Scope, missing :: Int) () { upper :: String }
b_upper = makeLayer do
  { missing } <- Om.ask
  pure { upper: show missing }

bad = wireLayers { a_base, b_upper }
