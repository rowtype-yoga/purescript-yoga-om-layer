-- EXPECT: TypesDoNotUnify
module Yoga.Om.Layer.CompileFailTest where

import Prelude
import Yoga.Om.Layer (OmLayer, Scope, makeLayer, combineRequirements, runScoped)
import Effect.Aff (Aff)

layer1 :: OmLayer (scope :: Scope) () { value :: Int }
layer1 = makeLayer (pure { value: 1 })

layer2 :: OmLayer (scope :: Scope) () { value :: String }
layer2 = makeLayer (pure { value: "two" })

bad :: Aff { value :: Int, value :: String }
bad = runScoped (combineRequirements layer1 layer2)
