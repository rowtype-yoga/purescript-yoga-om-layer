-- EXPECT: Prim.RowList.RowToList
module Yoga.Om.Layer.CompileFailTest where

import Prelude

import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer, runLayer)

type Fastify = String
type Connection = String

apiLayer :: forall r. OmLayer (fastify :: Fastify, sqlite :: Connection | r) () {}
apiLayer = makeLayer do
  { fastify, sqlite } <- Om.ask
  let _ = fastify <> sqlite
  pure {}

bad :: Om.Om { fastify :: Fastify } () {}
bad = runLayer { fastify: "server" } apiLayer
