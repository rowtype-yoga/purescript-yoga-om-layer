module Test.Yoga.Om.Layer.WireLayersSpec where

import Prelude

import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, Scope, makeLayer, runScoped, wireLayers)

spec :: Spec Unit
spec = describe "wireLayers" do

  it "wires a simple linear chain (A -> B -> C)" do
    let
      a_base :: OmLayer (scope :: Scope) () { base :: String }
      a_base = makeLayer (pure { base: "foundation" })

      b_mid :: OmLayer (scope :: Scope, base :: String) () { mid :: String }
      b_mid = makeLayer do
        { base } <- Om.ask
        pure { mid: base <> "-middle" }

      c_top :: OmLayer (scope :: Scope, mid :: String) () { top :: String }
      c_top = makeLayer do
        { mid } <- Om.ask
        pure { top: mid <> "-top" }

      app = wireLayers { a_base, b_mid, c_top }

    result <- liftAff $ runScoped app
    result.base `shouldEqual` "foundation"
    result.mid `shouldEqual` "foundation-middle"
    result.top `shouldEqual` "foundation-middle-top"

  it "wires independent layers with no inter-dependencies" do
    let
      a_x :: OmLayer (scope :: Scope) () { x :: Int }
      a_x = makeLayer (pure { x: 1 })

      b_y :: OmLayer (scope :: Scope) () { y :: Int }
      b_y = makeLayer (pure { y: 2 })

      c_z :: OmLayer (scope :: Scope) () { z :: Int }
      c_z = makeLayer (pure { z: 3 })

      app = wireLayers { a_x, b_y, c_z }

    result <- liftAff $ runScoped app
    result.x `shouldEqual` 1
    result.y `shouldEqual` 2
    result.z `shouldEqual` 3

  it "wires a diamond dependency" do
    let
      a_db :: OmLayer (scope :: Scope) () { db :: String }
      a_db = makeLayer (pure { db: "postgres" })

      b_cache :: OmLayer (scope :: Scope, db :: String) () { cache :: String }
      b_cache = makeLayer do
        { db } <- Om.ask
        pure { cache: db <> "-cache" }

      b_repo :: OmLayer (scope :: Scope, db :: String) () { repo :: String }
      b_repo = makeLayer do
        { db } <- Om.ask
        pure { repo: db <> "-repo" }

      d_app :: OmLayer (scope :: Scope, cache :: String, repo :: String) () { app :: String }
      d_app = makeLayer do
        { cache, repo } <- Om.ask
        pure { app: cache <> "+" <> repo }

      app = wireLayers { a_db, b_cache, b_repo, d_app }

    result <- liftAff $ runScoped app
    result.db `shouldEqual` "postgres"
    result.cache `shouldEqual` "postgres-cache"
    result.repo `shouldEqual` "postgres-repo"
    result.app `shouldEqual` "postgres-cache+postgres-repo"

  it "memoizes shared dependencies within wireLayers" do
    counter <- liftEffect $ Ref.new 0
    let
      a_db :: OmLayer (scope :: Scope) () { db :: String }
      a_db = makeLayer do
        liftEffect $ Ref.modify_ (_ + 1) counter
        pure { db: "connected" }

      b_cache :: OmLayer (scope :: Scope, db :: String) () { cache :: String }
      b_cache = makeLayer do
        { db } <- Om.ask
        pure { cache: db <> "-cache" }

      b_repo :: OmLayer (scope :: Scope, db :: String) () { repo :: String }
      b_repo = makeLayer do
        { db } <- Om.ask
        pure { repo: db <> "-repo" }

      app = wireLayers { a_db, b_cache, b_repo }

    result <- liftAff $ runScoped app
    result.cache `shouldEqual` "connected-cache"
    result.repo `shouldEqual` "connected-repo"
    count <- liftEffect $ Ref.read counter
    count `shouldEqual` 1
