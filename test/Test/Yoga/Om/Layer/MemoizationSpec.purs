module Test.Yoga.Om.Layer.MemoizationSpec where

import Prelude

import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, Scope, makeLayer, makeScopedLayer, bracketLayer, combineRequirements, runScoped, provide)

spec :: Spec Unit
spec = describe "Automatic Memoization" do

  it "builds the same layer only once when used in two branches" do
    counter <- liftEffect $ Ref.new 0
    let
      expensiveLayer :: OmLayer (scope :: Scope) () { value :: String }
      expensiveLayer = makeLayer do
        liftEffect $ Ref.modify_ (_ + 1) counter
        pure { value: "built" }

      branch1 :: OmLayer (scope :: Scope) () { out1 :: String }
      branch1 = consumer `provide` expensiveLayer
        where
        consumer :: OmLayer (scope :: Scope, value :: String) () { out1 :: String }
        consumer = makeLayer do
          { value } <- Om.ask
          pure { out1: value <> "-1" }

      branch2 :: OmLayer (scope :: Scope) () { out2 :: String }
      branch2 = consumer `provide` expensiveLayer
        where
        consumer :: OmLayer (scope :: Scope, value :: String) () { out2 :: String }
        consumer = makeLayer do
          { value } <- Om.ask
          pure { out2: value <> "-2" }

      app = combineRequirements branch1 branch2

    result <- liftAff $ runScoped app
    result.out1 `shouldEqual` "built-1"
    result.out2 `shouldEqual` "built-2"
    count <- liftEffect $ Ref.read counter
    count `shouldEqual` 1

  it "memoized scoped layer runs finalizer only once" do
    log <- liftEffect $ Ref.new []
    let
      dbLayer :: OmLayer (scope :: Scope) () { db :: String }
      dbLayer = makeScopedLayer
        (pure { db: "connected" })
        (\_ -> liftEffect $ Ref.modify_ (_ <> [ "db-closed" ]) log)

      repoBranch :: OmLayer (scope :: Scope) () { repo :: String }
      repoBranch = repo `provide` dbLayer
        where
        repo :: OmLayer (scope :: Scope, db :: String) () { repo :: String }
        repo = makeLayer do
          { db } <- Om.ask
          pure { repo: db <> "-repo" }

      analyticsBranch :: OmLayer (scope :: Scope) () { analytics :: String }
      analyticsBranch = analytics `provide` dbLayer
        where
        analytics :: OmLayer (scope :: Scope, db :: String) () { analytics :: String }
        analytics = makeLayer do
          { db } <- Om.ask
          pure { analytics: db <> "-analytics" }

      app = combineRequirements repoBranch analyticsBranch

    result <- liftAff $ runScoped app
    result.repo `shouldEqual` "connected-repo"
    result.analytics `shouldEqual` "connected-analytics"
    finalLog <- liftEffect $ Ref.read log
    finalLog `shouldEqual` [ "db-closed" ]

  it "memoized bracketLayer acquires and releases only once" do
    log <- liftEffect $ Ref.new []
    let
      connLayer :: OmLayer (scope :: Scope) () { conn :: Int }
      connLayer = bracketLayer
        ( do
            liftEffect $ Ref.modify_ (_ <> [ "acquire" ]) log
            pure 42
        )
        (\_ -> liftEffect $ Ref.modify_ (_ <> [ "release" ]) log)
        (\n -> pure { conn: n })

      branch1 :: OmLayer (scope :: Scope) () { x :: String }
      branch1 = upper `provide` connLayer
        where
        upper :: OmLayer (scope :: Scope, conn :: Int) () { x :: String }
        upper = makeLayer do
          { conn } <- Om.ask
          pure { x: show conn }

      branch2 :: OmLayer (scope :: Scope) () { y :: String }
      branch2 = upper `provide` connLayer
        where
        upper :: OmLayer (scope :: Scope, conn :: Int) () { y :: String }
        upper = makeLayer do
          { conn } <- Om.ask
          pure { y: show (conn + 1) }

      app = combineRequirements branch1 branch2

    result <- liftAff $ runScoped app
    result.x `shouldEqual` "42"
    result.y `shouldEqual` "43"
    finalLog <- liftEffect $ Ref.read log
    finalLog `shouldEqual` [ "acquire", "release" ]
