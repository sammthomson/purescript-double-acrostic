module Data.Bimap (
  Bimap,
  delete,
  empty,
  entryList,
  fromFoldable,
  insert,
  invert,
  isEmpty,
  lookup,
  map,
  mapAppend,
  mapEntries,
  singleton,
  toList
) where

import Prelude

import Data.Array as A
import Data.Foldable (class Foldable, foldl)
import Data.List.Lazy as LL
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), uncurry)


-- | An invertible map, where lookup can be done in either direction.
-- | The constructor should not be exported, so we can keep `forward` and
-- | `backward` consistent.
data Bimap k v = Bimap (M.Map k v) (M.Map v k)

invert :: forall k v. Bimap k v -> Bimap v k
invert (Bimap fwd bkwd) = Bimap bkwd fwd

empty :: forall k v. Bimap k v
empty = Bimap M.empty M.empty

-- | Test if a bimap is empty
isEmpty :: forall k v. Bimap k v -> Boolean
isEmpty (Bimap fwd _) = M.isEmpty fwd

-- | Create a bimap with one key/value pair
singleton :: forall k v. k -> v -> Bimap k v
singleton k v = Bimap (M.singleton k v) (M.singleton v k)

entryList :: forall k v. Bimap k v -> LL.List (Tuple k v)
entryList (Bimap fwd _) = M.toAscUnfoldable fwd

-- | Map `f` over the entries of `ma` then append the result to `mb`.
mapAppend :: forall k v w. Ord k => Ord v => Ord w =>
             (k -> v -> Tuple k w) ->
             Bimap k v ->
             Bimap k w ->
             Bimap k w
mapAppend f ma mb = foldl go mb $ uncurry f <$> entryList ma where
  go acc (Tuple k v) = insert k v acc


-- | Map `f` over the entries of `ma`.
mapEntries :: forall k v w. Ord k => Ord v => Ord w =>
              (k -> v -> Tuple k w) ->
              Bimap k v ->
              Bimap k w
mapEntries f ma = mapAppend f ma empty


-- | Not a valid `Functor` (for the same reason `Set` isn't), but useful anyway
map :: forall k v w. Ord k => Ord v => Ord w =>
       (v -> w) ->
       Bimap k v ->
       Bimap k w
map f = mapEntries (\k v -> Tuple k (f v))


-- | Lookup the value of `k`.
lookup :: forall k v. Ord k => k -> Bimap k v -> Maybe v
lookup k (Bimap fwd _) = M.lookup k fwd


-- | Insert or replace a key/value pair in a bimap.
insert :: forall k v. Ord k => Ord v => k -> v -> Bimap k v -> Bimap k v
insert k v (Bimap fwd bkwd) = Bimap (M.insert k v fwd) (M.insert v k bkwd)


-- | Delete a key and its corresponding value from a bimap.
delete :: forall k v. Ord k => Ord v => k -> Bimap k v -> Bimap k v
delete k m@(Bimap fwd bkwd) = case M.lookup k fwd of
   Just v -> Bimap (M.delete k fwd) (M.delete v bkwd)
   Nothing -> m


-- | Construct a `Bimap` from the given `Foldable`.
fromFoldable :: forall t k v. Foldable t => Ord k => Ord v =>
                t (Tuple k v) ->
                Bimap k v
fromFoldable = foldl (\m (Tuple k v) -> insert k v m) empty


toList :: forall k v. Bimap k v -> LL.List (Tuple k v)
toList (Bimap fwd _) = M.toAscUnfoldable fwd


derive instance eqBimap :: (Eq k, Eq v) => Eq (Bimap k v)


instance showBimap :: (Show k, Show v) => Show (Bimap k v) where
  show (Bimap m _) = "{" <> (joinWith ", " (entryStrs m)) <> "}" where
    entryStrs = A.fromFoldable <<< M.mapWithKey \k v -> show k <> ": " <> show v


instance semigroupBimap :: (Ord k, Ord v) => Semigroup (Bimap k v) where
  append = mapAppend Tuple


instance monoidBimap :: (Ord k, Ord v) => Monoid (Bimap k v) where
  mempty = empty
