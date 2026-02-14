-- @inline export runLayer arity=2
module Yoga.Om.Layer
  ( OmLayer
  , ParOmLayer
  , Scope
  , makeLayer
  , makeScopedLayer
  , bracketLayer
  , acquireRelease
  , fresh
  , runLayer
  , runScoped
  , runScopedWith
  , withScoped
  , withScopedWith
  , combineRequirements
  , provide
  , (>->)
  , recovering
  , repeating
  , wireLayers
  , wireLayersDebug
  , wireLayersRL
  , class WireLayersRL
  , class FilterScope
  , class FindProvider
  , class FindProviderMatch
  , class HasLabel
  , class EmitEdges
  , class PrintLayersRL
  , class PrintEdgesRL
  , class CheckAllProvided
  , class CheckAllLabelsExist
  , class CheckLabelExists
  ) where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Data.Symbol (class IsSymbol)
import Control.Parallel (class Parallel)
import Control.Parallel as Parallel
import Data.Array as Array
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Variant (class VariantMatchCases, onMatch)
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.Retry (RetryPolicyM, RetryStatus, applyAndDelay, defaultRetryStatus)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row (class Nub, class Union)
import Prim.Row as Row
import Type.Proxy (Proxy(..))
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Prim.Boolean (True, False)
import Prim.Symbol (class Append)
import Prim.TypeError (class Fail, class Warn, Above, Doc, Quote, Text)
import Record as Record
import Record.Studio (class Keys)
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Om (Om, ParOm)
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

-- | A layer that requires dependencies (req), may fail with (err),
-- | and produces a value (typically Record prov).
-- | Each layer has a unique identity used for automatic memoization within a Scope.
data OmLayer req err a = OmLayer Int (Om (Record req) err a)

-- =============================================================================
-- ParOmLayer — parallel counterpart
-- =============================================================================

data ParOmLayer req err a = ParOmLayer (ParOm (Record req) err a)

-- =============================================================================
-- Instances
-- =============================================================================

instance Functor (OmLayer req err) where
  map f layer = OmLayer (freshId unit) (map f (buildLayer layer))

instance Apply (OmLayer req err) where
  apply f a = OmLayer (freshId unit) (apply (buildLayer f) (buildLayer a))

instance Applicative (OmLayer req err) where
  pure a = OmLayer (freshId unit) (pure a)

instance Bind (OmLayer req err) where
  bind layer f = OmLayer (freshId unit) do
    a <- buildLayer layer
    buildLayer (f a)

instance Monad (OmLayer req err)

instance Functor (ParOmLayer req err) where
  map f (ParOmLayer p) = ParOmLayer (map f p)

instance Apply (ParOmLayer req err) where
  apply (ParOmLayer f) (ParOmLayer a) = ParOmLayer (apply f a)

instance Applicative (ParOmLayer req err) where
  pure a = ParOmLayer (pure a)

instance Parallel (ParOmLayer req err) (OmLayer req err) where
  parallel (OmLayer _ om) = ParOmLayer (Parallel.parallel om)
  sequential (ParOmLayer p) = OmLayer (freshId unit) (Parallel.sequential p)

-- =============================================================================
-- Automatic memoization
-- =============================================================================

-- | Wrap an Om computation with cache logic.
-- | If a scope is available in the context, checks/populates the cache by layer ID.
-- | If no scope is present, runs the computation directly.
withCache :: forall req err a. Int -> Om (Record req) err a -> Om (Record req) err a
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
makeLayer :: forall req err a. Om (Record req) err a -> OmLayer req err a
makeLayer om = OmLayer (freshId unit) om

-- | Extract the Om from a layer, wrapped with cache logic.
buildLayer :: forall req err a. OmLayer req err a -> Om (Record req) err a
buildLayer (OmLayer layerId om) = withCache layerId om

-- | Create a scoped layer with acquire/release semantics.
-- | The release function runs when the enclosing scope closes.
-- | The layer requires `scope :: Scope` in its context.
makeScopedLayer
  :: forall req prov err
   . Om { scope :: Scope | req } err (Record prov)
  -> (Record prov -> Aff Unit)
  -> OmLayer (scope :: Scope | req) err (Record prov)
makeScopedLayer acquire release = makeLayer do
  acquireRelease acquire release

-- | Bracket-style scoped layer: acquire a resource, register its release,
-- | then build provisions from it.
bracketLayer
  :: forall req prov resource err
   . Om { scope :: Scope | req } err resource
  -> (resource -> Aff Unit)
  -> (resource -> Om { scope :: Scope | req } err (Record prov))
  -> OmLayer (scope :: Scope | req) err (Record prov)
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
fresh :: forall req err a. OmLayer req err a -> OmLayer req err a
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
  :: forall req err a available
   . CheckAllProvided req available
  => Record available
  -> OmLayer req err a
  -> Om (Record available) err a
runLayer _ctx layer = widenCtx (buildLayer layer)
  where
  widenCtx :: Om (Record req) err a -> Om (Record available) err a
  widenCtx = unsafeCoerce

-- | Build a fully-provided scoped layer, return the provisions.
-- | All finalizers run after the provisions are returned.
-- | For layers with typed errors, use `runScopedWith`.
runScoped
  :: forall prov
   . OmLayer (scope :: Scope) () (Record prov)
  -> Aff (Record prov)
runScoped layer = withScoped layer pure

-- | Build a fully-provided scoped layer and pass the provisions to a callback.
-- | A fresh `Scope` is created and closed when the callback completes,
-- | running all finalizers in reverse order — whether by success, failure,
-- | or interruption.
-- | For layers with typed errors, use `withScopedWith`.
withScoped
  :: forall prov a
   . OmLayer (scope :: Scope) () (Record prov)
  -> (Record prov -> Aff a)
  -> Aff a
withScoped = withScopedWith { exception: \_ -> pure (unsafeCoerce {}) }

-- | Like `runScoped` but accepts error handlers for layers with typed errors.
runScopedWith
  :: forall prov r rl err_ err
   . RowToList (exception :: Error -> Aff (Record prov) | r) rl
  => VariantMatchCases rl err_ (Aff (Record prov))
  => Union err_ () (exception :: Error | err)
  => { exception :: Error -> Aff (Record prov) | r }
  -> OmLayer (scope :: Scope) err (Record prov)
  -> Aff (Record prov)
runScopedWith handlers layer = withScopedWith handlers layer pure

-- | Like `withScoped` but accepts error handlers for layers with typed errors.
withScopedWith
  :: forall prov a r rl err_ err
   . RowToList (exception :: Error -> Aff (Record prov) | r) rl
  => VariantMatchCases rl err_ (Aff (Record prov))
  => Union err_ () (exception :: Error | err)
  => { exception :: Error -> Aff (Record prov) | r }
  -> OmLayer (scope :: Scope) err (Record prov)
  -> (Record prov -> Aff a)
  -> Aff a
withScopedWith handlers layer callback = bracket acquire release use
  where
  acquire = do
    scope <- newScope # liftEffect
    let om = buildLayer layer
    prov <- Om.runOm { scope } handlers om
    pure { scope, prov }
  release { scope } = closeScope scope
  use { prov } = callback prov

-- =============================================================================
-- Horizontal composition — combine two layers with automatic deduplication
-- =============================================================================

combineRequirements
  :: forall req1 req2 prov1 prov2 err1 err2 provMerged reqMerged reqDeduped errMerged errDeduped _req1 _req2 _err1 _err2
   . Union req1 req2 reqMerged
  => Nub reqMerged reqDeduped
  => Union req1 _req1 reqDeduped
  => Union req2 _req2 reqDeduped
  => Union err1 err2 errMerged
  => Nub errMerged errDeduped
  => Union err1 _err1 errDeduped
  => Union err2 _err2 errDeduped
  => Union prov1 prov2 provMerged
  => Nub provMerged provMerged
  => Keys req1
  => Keys req2
  => OmLayer req1 err1 (Record prov1)
  -> OmLayer req2 err2 (Record prov2)
  -> OmLayer reqDeduped errDeduped (Record provMerged)
combineRequirements layer1 layer2 = OmLayer (freshId unit) do
  rec1 <- Om.expand (buildLayer layer1)
  rec2 <- Om.expand (buildLayer layer2)
  pure (Record.merge rec1 rec2)

-- =============================================================================
-- Vertical composition — feed output of one layer into input of another
-- =============================================================================

provide
  :: forall req prov1 prov2 err1 err2 req2 reqOut errMerged errDeduped _req _prov1 _err1 _err2
   . Union req _req req
  => Union prov1 _prov1 prov1
  => Union err1 err2 errMerged
  => Nub errMerged errDeduped
  => Union err1 _err1 errDeduped
  => Union err2 _err2 errDeduped
  => Union prov1 req reqOut
  => Nub reqOut reqOut
  => Union req2 _prov1 reqOut
  => Keys req
  => Keys prov1
  => Keys req2
  => OmLayer req2 err2 (Record prov2)
  -> OmLayer req err1 (Record prov1)
  -> OmLayer req errDeduped (Record prov2)
provide l2 l1 = OmLayer (freshId unit) do
  prov1 <- Om.expand (buildLayer l1)
  Om.expand (buildLayer l2) # Om.widenCtx prov1

infixl 9 provide as >->

-- =============================================================================
-- Retry / Repeat on layers
-- =============================================================================

recovering
  :: forall req err rest a
       (handlersRL :: RowList Type)
       (handlers :: Row Type)
       (handled :: Row Type)
   . RowToList handlers handlersRL
  => VariantMatchCases handlersRL handled (Om (Record req) err Boolean)
  => Union handled rest (exception :: Error | err)
  => RetryPolicyM (Om (Record req) err)
  -> (RetryStatus -> Record handlers)
  -> OmLayer req err a
  -> OmLayer req err a
recovering policy mkChecks (OmLayer layerId om) =
  OmLayer layerId (go defaultRetryStatus)
  where
  go status =
    catchError om (handleErr status)

  handleErr status variant = do
    let shouldRetry = variant # onMatch (mkChecks status) (\_ -> pure false)
    ifM shouldRetry
      (applyAndDelay policy status >>= maybe (throwError variant) go)
      (throwError variant)

repeating
  :: forall req err a
   . RetryPolicyM (Om (Record req) err)
  -> (RetryStatus -> a -> Om (Record req) err Boolean)
  -> OmLayer req err a
  -> OmLayer req err a
repeating policy shouldRepeat (OmLayer layerId om) =
  OmLayer layerId (go defaultRetryStatus)
  where
  go status = do
    result <- om
    ifM (shouldRepeat status result)
      (applyAndDelay policy status >>= maybe (pure result) go)
      (pure result)

-- =============================================================================
-- Auto-wiring
-- =============================================================================

class
  WireLayersRL
    (rl :: RowList Type)
    (layers :: Row Type)
    (accReq :: Row Type)
    (accErr :: Row Type)
    (accProv :: Row Type)
    (resReq :: Row Type)
    (resErr :: Row Type)
    (resProv :: Row Type)
  | rl layers accReq accErr accProv -> resReq resErr resProv
  where
  wireLayersRL
    :: Proxy rl
    -> Record layers
    -> OmLayer accReq accErr (Record accProv)
    -> OmLayer resReq resErr (Record resProv)

instance WireLayersRL Nil layers accReq accErr accProv accReq accErr accProv where
  wireLayersRL _ _ acc = acc

instance
  ( IsSymbol sym
  , Row.Cons sym (OmLayer nextReq nextErr (Record nextProv)) _rest layers
  -- next's requirements must be subset of (accProv + accReq)
  , Union accProv accReq provReqOut
  , Nub provReqOut provReqOut
  , Union nextReq _nextReqRest provReqOut
  , Keys nextReq
  , Keys accReq
  , Keys accProv
  -- merge errors
  , Union accErr nextErr errMergedRaw
  , Nub errMergedRaw errMerged
  , Union accErr _e1 errMerged
  , Union nextErr _e2 errMerged
  -- merge provisions (Nub provMerged provMerged ensures disjoint)
  , Union accProv nextProv provMerged
  , Nub provMerged provMerged
  -- recurse
  , WireLayersRL tail layers accReq errMerged provMerged resReq resErr resProv
  ) =>
  WireLayersRL (Cons sym (OmLayer nextReq nextErr (Record nextProv)) tail) layers accReq accErr accProv resReq resErr resProv
  where
  wireLayersRL _ layers acc =
    wireLayersRL (Proxy :: Proxy tail) layers combined
    where
    next :: OmLayer nextReq nextErr (Record nextProv)
    next = Record.get (Proxy :: Proxy sym) layers

    combined :: OmLayer accReq errMerged (Record provMerged)
    combined = OmLayer (freshId unit) do
      accProv <- Om.expandErr (buildLayer acc)
      nextProv <- Om.expand (buildLayer next) # Om.widenCtx accProv
      pure (Record.merge accProv nextProv)

wireLayers
  :: forall layers rl req err prov
   . RowToList layers rl
  => WireLayersRL rl layers (scope :: Scope) () () req err prov
  => Record layers
  -> OmLayer req err (Record prov)
wireLayers layers =
  wireLayersRL (Proxy :: Proxy rl) layers seed
  where
  seed = pure {} :: OmLayer (scope :: Scope) () (Record ())

-- =============================================================================
-- Debug graph printing (D2 diagram)
-- =============================================================================

-- | Remove `scope :: Scope` from a RowList (it's always present, just noise).
class FilterScope (rl :: RowList Type) (out :: RowList Type) | rl -> out

instance FilterScope Nil Nil
instance FilterScope (Cons "scope" Scope tail) tail
else instance FilterScope tail out => FilterScope (Cons sym ty tail) (Cons sym ty out)

-- | Given a label, find which layer in the RowList provides it.
class FindProvider (label :: Symbol) (rl :: RowList Type) (provider :: Symbol) | label rl -> provider

instance
  ( RowToList prov provRL
  , HasLabel label provRL hasIt
  , FindProviderMatch hasIt label sym (Cons sym (OmLayer req err (Record prov)) tail) provider
  ) =>
  FindProvider label (Cons sym (OmLayer req err (Record prov)) tail) provider

-- | Dispatch based on whether the current layer has the label.
class FindProviderMatch (hasIt :: Boolean) (label :: Symbol) (sym :: Symbol) (rl :: RowList Type) (provider :: Symbol) | hasIt label sym rl -> provider

instance FindProviderMatch True label sym rl sym

else instance
  FindProvider label tail provider =>
  FindProviderMatch False label sym (Cons sym ty tail) provider

-- | Check if a RowList contains a given label.
class HasLabel (label :: Symbol) (rl :: RowList Type) (result :: Boolean) | label rl -> result

instance HasLabel label Nil False
instance HasLabel label (Cons label ty tail) True
else instance HasLabel label tail result => HasLabel label (Cons sym ty tail) result

-- | Emit D2 edges for one consumer: "provider -> consumer" per requirement.
class EmitEdges (consumer :: Symbol) (reqRL :: RowList Type) (allLayers :: RowList Type) (doc :: Doc) | consumer reqRL allLayers -> doc

instance EmitEdges consumer Nil allLayers (Text "")

else instance
  ( FindProvider label allLayers provider
  , Append provider " -> " s1
  , Append s1 consumer line
  , EmitEdges consumer tail allLayers restDoc
  ) =>
  EmitEdges consumer (Cons label ty tail) allLayers (Above (Text line) restDoc)

-- | Walk all layers emitting D2 edges.
class PrintLayersRL (rl :: RowList Type) (doc :: Doc) | rl -> doc
class PrintEdgesRL (rl :: RowList Type) (allLayers :: RowList Type) (doc :: Doc) | rl allLayers -> doc

instance PrintLayersRL Nil (Text "")
else instance PrintEdgesRL rl rl doc => PrintLayersRL rl doc

instance PrintEdgesRL Nil allLayers (Text "")

else instance
  ( RowToList req reqRL
  , FilterScope reqRL filteredReqRL
  , EmitEdges sym filteredReqRL allLayers edgeDoc
  , PrintEdgesRL tail allLayers restDoc
  ) =>
  PrintEdgesRL (Cons sym (OmLayer req err (Record prov)) tail) allLayers (Above edgeDoc restDoc)

wireLayersDebug
  :: forall layers rl req err prov doc
   . RowToList layers rl
  => PrintLayersRL rl doc
  => Warn (Above (Text "") (Above (Text "Copy into https://play.d2lang.com") doc))
  => WireLayersRL rl layers (scope :: Scope) () () req err prov
  => Record layers
  -> OmLayer req err (Record prov)
wireLayersDebug = wireLayers
