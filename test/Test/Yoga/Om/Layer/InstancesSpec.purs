module Test.Yoga.Om.Layer.InstancesSpec where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (ask, local)
import Control.Parallel (parApply)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Effect.Exception as Exception
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer, runLayer)

spec :: Spec Unit
spec = do

  describe "Functor / Apply / Bind" do

    it "maps over layer output" do
      let
        layer :: OmLayer () () { value :: Int }
        layer = makeLayer (pure { value: 10 })

        mapped = layer <#> \r -> { doubled: r.value * 2 }

      result <- Om.runOm {} { exception: \_ -> pure { doubled: 0 } } (runLayer {} mapped)
      result.doubled `shouldEqual` 20

    it "applies two layers" do
      let
        fLayer :: OmLayer () () ({ a :: Int } -> { a :: Int, b :: String })
        fLayer = makeLayer (pure \r -> { a: r.a, b: show r.a })

        aLayer :: OmLayer () () { a :: Int }
        aLayer = makeLayer (pure { a: 42 })

      result <- Om.runOm {} { exception: \_ -> pure { a: 0, b: "" } } (runLayer {} (fLayer <*> aLayer))
      result.a `shouldEqual` 42
      result.b `shouldEqual` "42"

    it "binds layers sequentially" do
      let
        layer = do
          r <- makeLayer (pure { x: 5 }) :: OmLayer () () { x :: Int }
          makeLayer (pure { result: r.x + 10 })

      result <- Om.runOm {} { exception: \_ -> pure { result: 0 } } (runLayer {} layer)
      result.result `shouldEqual` 15

    it "runs layers against a superset context" do
      let
        layer :: OmLayer (a :: Int) () { out :: Int }
        layer = makeLayer do
          { a } <- Om.ask
          pure { out: a + 1 }

        ctx = { a: 41, extra: "ok" }

      result <- Om.runOm ctx { exception: \_ -> pure { out: 0 } } (runLayer ctx layer)
      result.out `shouldEqual` 42

  describe "MonadEffect / MonadAff" do

    it "liftEffect works directly in layer bind" do
      ref <- liftEffect $ Ref.new 0
      let
        layer :: OmLayer () () Int
        layer = do
          liftEffect $ Ref.write 42 ref
          liftEffect $ Ref.read ref

      result <- Om.runOm {} { exception: \_ -> pure 0 } (runLayer {} layer)
      result `shouldEqual` 42

  describe "MonadAsk / MonadReader" do

    it "ask reads the context directly" do
      let
        layer :: OmLayer (port :: Int) () Int
        layer = do
          ctx <- ask
          pure ctx.port

      result <- Om.runOm { port: 8080 } { exception: \_ -> pure 0 } (runLayer { port: 8080 } layer)
      result `shouldEqual` 8080

    it "local modifies context for a layer" do
      let
        inner :: OmLayer (port :: Int) () Int
        inner = do
          ctx <- ask
          pure ctx.port

        layer :: OmLayer (port :: Int) () Int
        layer = local (\r -> r { port = r.port + 1 }) inner

      result <- Om.runOm { port: 8080 } { exception: \_ -> pure 0 } (runLayer { port: 8080 } layer)
      result `shouldEqual` 8081

  describe "MonadThrow / MonadError" do

    it "throwError raises into the error channel" do
      let
        layer :: OmLayer () (myErr :: String) Int
        layer = Om.throw { myErr: "boom" }

      result <- Om.runOm {}
        { exception: \_ -> pure (-1), myErr: \_ -> pure (-2) }
        (runLayer {} layer)
      result `shouldEqual` (-2)

  describe "Alt" do

    it "falls back to second layer on failure" do
      let
        failing :: OmLayer () () Int
        failing = Om.throw { exception: Exception.error "nope" }

        fallback :: OmLayer () () Int
        fallback = pure 99

        layer = failing <|> fallback

      result <- Om.runOm {} { exception: \_ -> pure (-1) } (runLayer {} layer)
      result `shouldEqual` 99

  describe "Semigroup" do

    it "appends layer outputs" do
      let
        a :: OmLayer () () String
        a = pure "hello"

        b :: OmLayer () () String
        b = pure " world"

      result <- Om.runOm {} { exception: \_ -> pure "" } (runLayer {} (a <> b))
      result `shouldEqual` "hello world"

  describe "Parallel" do

    it "runs layers in parallel via parApply" do
      log <- liftEffect $ Ref.new []
      let
        layerA :: OmLayer () () { a :: String }
        layerA = makeLayer do
          liftEffect $ Ref.modify_ (_ <> [ "a" ]) log
          pure { a: "A" }

        layerB :: OmLayer () () { b :: String }
        layerB = makeLayer do
          liftEffect $ Ref.modify_ (_ <> [ "b" ]) log
          pure { b: "B" }

        combined = parApply (map (\ra rb -> { a: ra.a, b: rb.b }) layerA) layerB

      result <- Om.runOm {} { exception: \_ -> pure { a: "", b: "" } } (runLayer {} combined)
      result.a `shouldEqual` "A"
      result.b `shouldEqual` "B"
      finalLog <- liftEffect $ Ref.read log
      finalLog `shouldEqual` [ "a", "b" ]
