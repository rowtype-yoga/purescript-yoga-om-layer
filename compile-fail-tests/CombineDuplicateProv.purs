-- EXPECT: TypesDoNotUnify
module Yoga.Om.Layer.CompileFailTest where

import Prelude
import Yoga.Om.Layer (OmLayer, Scope, makeLayer, combineRequirements, runScoped)

layer1 :: OmLayer (scope :: Scope) () { value :: Int }
layer1 = makeLayer (pure { value: 1 })

layer2 :: OmLayer (scope :: Scope) () { value :: Int }
layer2 = makeLayer (pure { value: 2 })

bad = runScoped (combineRequirements layer1 layer2)
