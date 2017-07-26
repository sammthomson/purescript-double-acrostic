module Data.Multiset where

import Prelude

import Data.Array as A
import Data.Foldable (class Foldable, foldl)
import Data.Group (class Group)
import Data.List.Lazy as LL
import Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Data.Semigroup.Commutative (class Commutative)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))


newtype Multiset a = Multiset (M.Map a Int)


empty :: forall a. Multiset a
empty = Multiset M.empty


entryList :: forall a. Multiset a -> LL.List (Tuple a Int)
entryList (Multiset m) = M.toAscUnfoldable m


-- | Map `f` over the entries of `ma` then append the result to `mb`.
mapAppend :: forall a b. Ord b =>
             (Tuple a Int -> Tuple b Int) ->
             Multiset a ->
             Multiset b ->
             Multiset b
mapAppend f ma mb = foldl go mb $ f <$> entryList ma where
  go acc (Tuple k v) = insertN v k acc


-- | Map `f` over the entries of `ma`.
mapEntries :: forall a b. Ord b =>
              (Tuple a Int -> Tuple b Int) ->
              Multiset a ->
              Multiset b
mapEntries f ma = mapAppend f ma empty


-- | Not a valid `Functor` (for the same reason `Set` isn't), but useful anyway
map :: forall a b. Ord a => Ord b => (a -> b) -> Multiset a -> Multiset b
map f = mapEntries (\(Tuple k v) -> Tuple (f k) v)


-- | Lookup the count of `k`.
freq :: forall a. Ord a => a -> Multiset a -> Int
freq k (Multiset m) = fromMaybe 0 (M.lookup k m)


-- | Set the count of of `k` to be `v`.
set :: forall a. Ord a => a -> Int -> Multiset a -> Multiset a
set k v (Multiset m) =
  Multiset $ if v == 0 then M.delete k m else M.insert k v m


-- | Insert `n` copies of `k`.
insertN :: forall a. Ord a => Int -> a -> Multiset a -> Multiset a
insertN n k m = set k (freq k m + n) m


-- | Insert one `k`.
insert :: forall a. Ord a => a -> Multiset a -> Multiset a
insert = insertN 1


-- | Remove `n` copies of `k`.
deleteN :: forall a. Ord a => Int -> a -> Multiset a -> Multiset a
deleteN n = insertN (-n)


-- | Remove one `k`.
delete :: forall a. Ord a => a -> Multiset a -> Multiset a
delete = deleteN 1


-- | Construct a `Multiset` from the given `Foldable`.
fromFoldable :: forall t a. Foldable t => Eq a => Ord a => t a -> Multiset a
fromFoldable = foldl (flip insert) empty


toList :: forall a. Multiset a -> LL.List a
toList (Multiset m) = M.toAscUnfoldable m >>= (\(Tuple k v) -> LL.replicate v k)


toArray :: forall a. Ord a => Multiset a -> Array a
toArray s = A.fromFoldable (toList s)


derive instance newtypeMultiset :: Newtype (Multiset a) _


derive instance eqMultiset :: Eq a => Eq (Multiset a)


instance showMultiset :: Show a => Show (Multiset a) where
  show (Multiset m) = "{" <> (joinWith ", " (entryStrs m)) <> "}" where
    entryStrs = A.fromFoldable <<< M.mapWithKey (\k v -> show k <> ": " <> show v)


instance semigroupMultiset :: Ord a => Semigroup (Multiset a) where
  append = mapAppend id


instance commutativeMultiset :: Ord a => Commutative (Multiset a)


instance monoidMultiset :: Ord a => Monoid (Multiset a) where
  mempty = empty


instance groupMultiset :: Ord a => Group (Multiset a) where
  ginverse = mapEntries (\(Tuple k v) -> Tuple k (-v))


instance foldableMultiset :: Foldable Multiset where
  foldr f z = toList >>> LL.foldr f z
  foldl f z = toList >>> LL.foldl f z
  foldMap f = toList >>> LL.foldMap f
