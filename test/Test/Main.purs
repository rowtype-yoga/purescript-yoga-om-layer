module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = launchAff_ do
  specs <- discover "Test\\.Yoga\\.Om\\.Layer\\..*Spec"
  liftEffect $ runSpecAndExitProcess [ consoleReporter ] specs
