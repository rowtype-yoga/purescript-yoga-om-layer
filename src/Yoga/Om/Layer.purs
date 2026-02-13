-- @inline export runLayer arity=2
module Yoga.Om.Layer
  ( OmLayer
  , Scope
  , makeLayer
  , makeScopedLayer
  , bracketLayer
  , acquireRelease
  , fresh
  , runLayer
  , runScoped
  , withScoped
  , combineRequirements
  , provide
  , (>->)
  , class CheckAllProvided
  , class CheckAllLabelsExist
  , class CheckLabelExists
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row (class Nub, class Union)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.TypeError (class Fail, Above, Quote, Text)
import Record as Record
import Record.Studio (class Keys)
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Om (Om)
import Yoga.Om as Om

-- =============================================================================
-- Scope — a first-class representation of resource lifetime
-- =============================================================================

-- | A first-class representation of resource lifetime and memoization.
-- | Finalizers are registered via `acquireRelease` and run in reverse order
-- | when the scope is closed. Layers are automatically memoized by identity
-- | within a scope — the same layer value builds only once.
newtype Scope = Scope
  { finalizers :: Ref (Array (Aff Unit))
  , cache :: Ref (Map Int Dynamic)
  }

-- | Opaque box for cached values, retrieved via `unsafeCoerce`.
foreign import data Dynamic :: Type

toDynamic :: forall a. a -> Dynamic
toDynamic = unsafeCoerce

fromDynamic :: forall a. Dynamic -> a
fromDynamic = unsafeCoerce

-- | Create a fresh, empty scope.
newScope :: Effect Scope
newScope = do
  finalizers <- Ref.new []
  cache <- Ref.new Map.empty
  pure (Scope { finalizers, cache })

-- | Register a finalizer to run when this scope closes.
addFinalizer :: Scope -> Aff Unit -> Effect Unit
addFinalizer (Scope s) fin = Ref.modify_ (_ <> [ fin ]) s.finalizers

-- | Close the scope, running all registered finalizers in reverse order.
closeScope :: Scope -> Aff Unit
closeScope (Scope s) = do
  fins <- liftEffect $ Ref.read s.finalizers
  for_ (Array.reverse fins) identity

-- =============================================================================
-- Unique layer IDs
-- =============================================================================

foreign import nextId :: Effect Int
foreign import tryGetScope :: forall r. Record r -> Nullable Scope

-- | Generate a unique layer ID. Uses unsafePerformEffect since layer
-- | construction is at module init time (like Scala's `object` identity).
-- | This is a function (Unit -> Int) to ensure each call site gets a unique ID.
freshId :: Unit -> Int
freshId _ = unsafePerformEffect nextId

-- =============================================================================
-- OmLayer
-- =============================================================================

-- | A layer that requires dependencies (req) and provides services (prov).
-- | Each layer has a unique identity used for automatic memoization within a Scope.
data OmLayer req prov err = OmLayer Int (Om (Record req) err (Record prov))

-- =============================================================================
-- Automatic memoization
-- =============================================================================

-- | Wrap an Om computation with cache logic.
-- | If a scope is available in the context, checks/populates the cache by layer ID.
-- | If no scope is present, runs the computation directly.
withCache :: forall req prov err. Int -> Om (Record req) err (Record prov) -> Om (Record req) err (Record prov)
withCache layerId om = do
  ctx <- Om.ask
  case toMaybe (tryGetScope ctx) of
    Nothing -> om
    Just (Scope s) -> do
      cached <- liftEffect $ Map.lookup layerId <$> Ref.read s.cache
      case cached of
        Just hit -> pure (fromDynamic hit)
        Nothing -> do
          prov <- om
          liftEffect $ Ref.modify_ (Map.insert layerId (toDynamic prov)) s.cache
          pure prov

-- =============================================================================
-- Layer constructors
-- =============================================================================

-- | Create a layer from an Om computation.
-- | The layer is automatically memoized within a Scope — the same layer value
-- | used in multiple branches of a dependency graph builds only once.
makeLayer :: forall req prov err. Om (Record req) err (Record prov) -> OmLayer req prov err
makeLayer om = OmLayer (freshId unit) om

-- | Extract the Om from a layer, wrapped with cache logic.
buildLayer :: forall req prov err. OmLayer req prov err -> Om (Record req) err (Record prov)
buildLayer (OmLayer layerId om) = withCache layerId om

-- | Create a scoped layer with acquire/release semantics.
-- | The release function runs when the enclosing scope closes.
-- | The layer requires `scope :: Scope` in its context.
makeScopedLayer
  :: forall req prov err
   . Om { scope :: Scope | req } err (Record prov)
  -> (Record prov -> Aff Unit)
  -> OmLayer (scope :: Scope | req) prov err
makeScopedLayer acquire release = makeLayer do
  acquireRelease acquire release

-- | Bracket-style scoped layer: acquire a resource, register its release,
-- | then build provisions from it.
bracketLayer
  :: forall req prov resource err
   . Om { scope :: Scope | req } err resource
  -> (resource -> Aff Unit)
  -> (resource -> Om { scope :: Scope | req } err (Record prov))
  -> OmLayer (scope :: Scope | req) prov err
bracketLayer acquire release use = makeLayer do
  resource <- acquireRelease acquire (\r -> release r)
  use resource

-- | Acquire a resource and register its release with the current scope.
-- | This is the core scoped resource primitive — the PureScript equivalent
-- | of ZIO's `ZIO.acquireRelease`.
acquireRelease
  :: forall ctx err a
   . Om { scope :: Scope | ctx } err a
  -> (a -> Aff Unit)
  -> Om { scope :: Scope | ctx } err a
acquireRelease acquire release = do
  a <- acquire
  { scope } <- Om.ask
  addFinalizer scope (release a) # liftEffect
  pure a

-- | Create a fresh (non-memoized) copy of a layer.
-- | The new layer has a unique identity and will always build independently,
-- | even within the same scope.
fresh :: forall req prov err. OmLayer req prov err -> OmLayer req prov err
fresh (OmLayer _ om) = OmLayer (freshId unit) om

-- =============================================================================
-- Custom Type Errors for Missing Dependencies
-- =============================================================================

-- | Check if all required dependencies are provided.
-- If not, show a helpful diff of required vs available.
class CheckAllProvided (required :: Row Type) (available :: Row Type)

instance
  ( RowToList required reqList
  , RowToList available availList
  , CheckAllLabelsExist reqList availList required available
  ) =>
  CheckAllProvided required available

-- | Check that all labels in required RowList exist in available RowList.
-- Pass along the original rows for better error messages.
class CheckAllLabelsExist (required :: RowList Type) (available :: RowList Type) (requiredRow :: Row Type) (availableRow :: Row Type)

instance CheckAllLabelsExist Nil available requiredRow availableRow

instance
  ( CheckLabelExists label ty available requiredRow availableRow
  , CheckAllLabelsExist tail available requiredRow availableRow
  ) =>
  CheckAllLabelsExist (Cons label ty tail) available requiredRow availableRow

-- | Check if a single label exists in the available RowList.
class CheckLabelExists (label :: Symbol) (ty :: Type) (available :: RowList Type) (requiredRow :: Row Type) (availableRow :: Row Type)

instance
  TypeEquals ty ty' =>
  CheckLabelExists label ty (Cons label ty' tail) requiredRow availableRow

else instance
  CheckLabelExists label ty tail requiredRow availableRow =>
  CheckLabelExists label ty (Cons otherLabel otherTy tail) requiredRow availableRow

else instance
  Fail
    ( Above
        (Text "Missing required dependency!")
        ( Above
            (Text "")
            ( Above
                (Text "The first missing field is: ")
                ( Above
                    (Quote label)
                    ( Above
                        (Text "")
                        ( Above
                            (Text "The layer requires: ")
                            ( Above
                                (Quote requiredRow)
                                ( Above
                                    (Text "")
                                    ( Above
                                        (Text "But you provided: ")
                                        (Quote availableRow)
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    ) =>
  CheckLabelExists label ty Nil requiredRow availableRow

-- =============================================================================
-- Running layers
-- =============================================================================

-- | Run a layer with a given context, with custom error if requirements aren't met.
runLayer
  :: forall req prov err available
   . CheckAllProvided req available
  => Record available
  -> OmLayer req prov err
  -> Om (Record available) err (Record prov)
runLayer _ctx layer = widenCtx (buildLayer layer)
  where
  widenCtx :: Om (Record req) err (Record prov) -> Om (Record available) err (Record prov)
  widenCtx = unsafeCoerce

-- | Build a fully-provided scoped layer, return the provisions.
-- | All finalizers run after the provisions are returned.
runScoped
  :: forall prov
   . OmLayer (scope :: Scope) prov ()
  -> Aff (Record prov)
runScoped layer = withScoped layer pure

-- | Build a fully-provided scoped layer and pass the provisions to a callback.
-- | A fresh `Scope` is created and closed when the callback completes,
-- | running all finalizers in reverse order — whether by success, failure,
-- | or interruption.
withScoped
  :: forall prov a
   . OmLayer (scope :: Scope) prov ()
  -> (Record prov -> Aff a)
  -> Aff a
withScoped layer callback = bracket acquire release use
  where
  acquire = do
    scope <- newScope # liftEffect
    let om = buildLayer layer
    prov <- Om.runOm { scope } { exception: \_ -> pure (unsafeCoerce {}) } om
    pure { scope, prov }
  release { scope } = closeScope scope
  use { prov } = callback prov

-- =============================================================================
-- Horizontal composition — combine two layers with automatic deduplication
-- =============================================================================

combineRequirements
  :: forall req1 req2 prov1 prov2 err1 err2 provMerged provMergedRaw reqMerged reqDeduped _req1 _req2 _err1 _err2
   . Union req1 req2 reqMerged
  => Nub reqMerged reqDeduped
  => Union req1 _req1 reqDeduped
  => Union req2 _req2 reqDeduped
  => Union err1 _err1 ()
  => Union err2 _err2 ()
  => Union prov1 prov2 provMergedRaw
  => Nub provMergedRaw provMerged
  => Keys req1
  => Keys req2
  => OmLayer req1 prov1 err1
  -> OmLayer req2 prov2 err2
  -> OmLayer reqDeduped provMerged ()
combineRequirements layer1 layer2 = OmLayer (freshId unit) do
  rec1 <- Om.expand (buildLayer layer1)
  rec2 <- Om.expand (buildLayer layer2)
  pure (Record.merge rec1 rec2)

-- =============================================================================
-- Vertical composition — feed output of one layer into input of another
-- =============================================================================

provide
  :: forall req prov1 prov2 err1 err2 req2 reqOut _req _prov1 _err1 _err2
   . Union req _req req
  => Union prov1 _prov1 prov1
  => Union err1 _err1 ()
  => Union err2 _err2 ()
  => Union prov1 req reqOut
  => Nub reqOut reqOut
  => Union req2 _prov1 reqOut
  => Keys req
  => Keys prov1
  => Keys req2
  => OmLayer req2 prov2 err2
  -> OmLayer req prov1 err1
  -> OmLayer req prov2 ()
provide l2 l1 = OmLayer (freshId unit) do
  prov1 <- Om.expand (buildLayer l1)
  Om.expand (buildLayer l2) # Om.widenCtx prov1

infixl 9 provide as >->
