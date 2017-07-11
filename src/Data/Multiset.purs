module Data.Multiset where

import Prelude (class Eq, class Ord, class Semigroup, class Show, flip, show, (+), (-), (<$>), (<<<), (<>), (>), (>>=), (>>>))
import Data.Unfoldable (class Unfoldable)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Array as A
import Data.List.Lazy as LL
import Data.Map as M
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (class Monoid)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))


newtype Multiset a = Multiset (M.Map a Int)


empty :: forall a. Multiset a
empty = Multiset M.empty


-- | Not a valid `Functor` (because of `Ord`?), but useful anyway
map :: forall a b. Ord a => Ord b => (a -> b) -> Multiset a -> Multiset b
map f s = foldl (\acc iN -> iN acc) empty inserts where
  inserts :: LL.List (Multiset b -> Multiset b)
  inserts = (\(Tuple k v) -> insertN v (f k)) <$> unfolded
  unfolded :: LL.List (Tuple a Int)
  unfolded = toUnfoldable s


toUnfoldable :: forall t a. Ord a => Unfoldable t => Multiset a -> t (Tuple a Int)
toUnfoldable s = M.toAscUnfoldable (unwrap s)


freq :: forall a. Ord a => a -> Multiset a -> Int
freq k (Multiset m) = fromMaybe 0 (M.lookup k m)


insertN :: forall a. Ord a => Int -> a -> Multiset a -> Multiset a
insertN n x s = wrap (M.alter addN x (unwrap s)) where
  addN = fromMaybe 0 >>> (+) n >>> Just


insert :: forall a. Ord a => a -> Multiset a -> Multiset a
insert = insertN 1


deleteN :: forall a. Ord a => Int -> a -> Multiset a -> Multiset a
deleteN n c (Multiset m) = Multiset (M.update minusN c m) where
  minusN v = if (v > n) then Just (v - n) else Nothing


delete :: forall a. Ord a => a -> Multiset a -> Multiset a
delete = deleteN 1


fromFoldable :: forall t a. Foldable t => Eq a => Ord a => t a -> Multiset a
fromFoldable = foldl (flip insert) empty


toList :: forall a. Multiset a -> LL.List a
toList (Multiset m) = M.toUnfoldable m >>= (\(Tuple k v) -> LL.replicate v k)


toArray :: forall a. Ord a => Multiset a -> Array a
toArray s = A.fromFoldable (toList s)


derive instance newtypeMultiset :: Newtype (Multiset a) _


derive instance eqMultiset :: Eq a => Eq (Multiset a)


instance showMultiset :: Show a => Show (Multiset a) where
  show (Multiset m) = "{" <> (joinWith ", " (entryStrs m)) <> "}" where
    entryStrs = A.fromFoldable <<< M.mapWithKey (\k v -> show k <> ": " <> show v)


instance semigroupMultiset :: Ord a => Semigroup (Multiset a) where
  append (Multiset a) (Multiset b) = Multiset (M.unionWith (+) a b)


instance monoidMultiset :: Ord a => Monoid (Multiset a) where
  mempty = empty


instance foldableMultiset :: Foldable Multiset where
  foldr f z = toList >>> LL.foldr f z
  foldl f z = toList >>> LL.foldl f z
  foldMap f = toList >>> LL.foldMap f
