-- @inline export makeLayer arity=1
-- @inline export runLayer arity=2
module Yoga.Om.Layer
  ( OmLayer
  , Finalizers
  , makeLayer
  , makeScopedLayer
  , bracketLayer
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
import Effect.Aff (Aff, generalBracket)
import Effect.Aff.Class (liftAff)
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
-- Finalizers — guaranteed cleanup actions collected during layer construction
-- =============================================================================

-- | Cleanup actions to run when a scope closes. Stored in construction order;
-- | executed in reverse (LIFO) by `runScoped`.
newtype Finalizers = Finalizers (Array (Aff Unit))

instance Semigroup Finalizers where
  append (Finalizers a) (Finalizers b) = Finalizers (a <> b)

instance Monoid Finalizers where
  mempty = Finalizers []

-- =============================================================================
-- OmLayer — a layer that requires dependencies, provides services, and
--           carries finalizers for resource cleanup
-- =============================================================================

type Built prov = { provisions :: Record prov, finalizers :: Finalizers }

-- | A layer that requires dependencies (req), provides services (prov),
-- | and may carry finalizers for scoped resource management.
data OmLayer req prov err = OmLayer (Om (Record req) err (Built prov))

-- =============================================================================
-- Layer constructors
-- =============================================================================

-- | Create a layer from an Om computation (no finalizers).
makeLayer :: forall req prov err. Om (Record req) err (Record prov) -> OmLayer req prov err
makeLayer om = OmLayer (om <#> \provisions -> { provisions, finalizers: mempty })

-- | Create a scoped layer with acquire/release semantics.
-- | The release function is guaranteed to run when `runScoped` closes the scope.
makeScopedLayer
  :: forall req prov err
   . Om (Record req) err (Record prov)
  -> (Record prov -> Aff Unit)
  -> OmLayer req prov err
makeScopedLayer acquire release = OmLayer do
  provisions <- acquire
  let finalizers = Finalizers [ release provisions ]
  pure { provisions, finalizers }

-- | Bracket-style scoped layer: acquire a resource, build provisions from it,
-- | and register a finalizer for the resource.
bracketLayer
  :: forall req prov resource err
   . Om (Record req) err resource
  -> (resource -> Aff Unit)
  -> (resource -> Om (Record req) err (Record prov))
  -> OmLayer req prov err
bracketLayer acquire release use = OmLayer do
  resource <- acquire
  provisions <- use resource
  let finalizers = Finalizers [ release resource ]
  pure { provisions, finalizers }

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

-- | Run a layer with a given context, discarding finalizers.
-- | Use `runScoped` instead if the layer has scoped resources.
runLayer
  :: forall req prov err available
   . CheckAllProvided req available
  => Record available
  -> OmLayer req prov err
  -> Om (Record available) err (Record prov)
runLayer _ctx (OmLayer om) = widenCtx om <#> _.provisions
  where
  widenCtx :: Om (Record req) err (Built prov) -> Om (Record available) err (Built prov)
  widenCtx = unsafeCoerce

-- | Build a fully-provided layer, run finalizers, return the provisions.
-- | Useful when you just need the built services.
runScoped
  :: forall prov
   . OmLayer () prov ()
  -> Aff (Record prov)
runScoped layer = withScoped layer pure

-- | Build a fully-provided layer and pass the provisions to a callback.
-- | All finalizers run in reverse order when the callback completes,
-- | whether by success, failure, or interruption.
withScoped
  :: forall prov a
   . OmLayer () prov ()
  -> (Record prov -> Aff a)
  -> Aff a
withScoped (OmLayer om) callback = generalBracket acquire conditions use
  where
  acquire = Om.runOm {} errorHandlers om
  errorHandlers = { exception: \_ -> pure { provisions: unsafeCoerce {}, finalizers: mempty } }
  conditions =
    { completed: \_ built -> runFinalizers built.finalizers
    , failed: \_ built -> runFinalizers built.finalizers
    , killed: \_ built -> runFinalizers built.finalizers
    }
  use built = callback built.provisions

-- | Run finalizers in reverse order (LIFO).
runFinalizers :: Finalizers -> Aff Unit
runFinalizers (Finalizers fs) = for_ (Array.reverse fs) identity

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
combineRequirements (OmLayer build1) (OmLayer build2) = OmLayer do
  built1 <- Om.expand build1
  built2 <- Om.expand build2
  let provisions = Record.merge built1.provisions built2.provisions
  let finalizers = built1.finalizers <> built2.finalizers
  pure { provisions, finalizers }

-- =============================================================================
-- Vertical composition — feed output of one layer into input of another
-- =============================================================================

provide
  :: forall req prov1 prov2 err1 err2 _req _prov1 _err1 _err2
   . Union req _req req
  => Union prov1 _prov1 prov1
  => Union err1 _err1 ()
  => Union err2 _err2 ()
  => Keys req
  => Keys prov1
  => OmLayer prov1 prov2 err2
  -> OmLayer req prov1 err1
  -> OmLayer req prov2 ()
provide (OmLayer layer2) (OmLayer layer1) = OmLayer do
  built1 <- Om.expand layer1
  built2 <- runLayer2 built1.provisions
  let finalizers = built1.finalizers <> built2.finalizers
  pure { provisions: built2.provisions, finalizers }
  where
  runLayer2 ctx = liftAff $ Om.runOm ctx errorHandlers (Om.expand layer2)
  errorHandlers = { exception: \_ -> pure { provisions: unsafeCoerce {}, finalizers: mempty } }

infixl 9 provide as >->
