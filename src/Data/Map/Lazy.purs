module Data.Map.Lazy where

import Prelude (class Ord, class Show, Ordering(..), compare, const, map, pure, show, ($), (+), (<<<), (<>), (>>>))
import Data.Array as A
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Control.Lazy as Z
import Data.List.Lazy as L

-- the weight-balancing threshold
alpha :: Number
alpha = 0.24

-- | A lazy approximately--weight-balanced tree
newtype Map k v = Map (Lazy (Node k v))

data Node k v
  = Leaf
  | Bin
    { key :: k
    , value :: v
    , left :: Map k v
    , right :: Map k v
    , size :: Lazy Int
    }

derive instance newtypeMap :: Newtype (Map k v) _

-- | Unwrap a lazy map
step :: forall k v. Map k v -> Node k v
step = force <<< unwrap

-- | An empty map
empty :: forall k v. Map k v
empty = Map $ defer \_ -> Leaf

-- | Test if a map is empty
isEmpty :: forall k v. Map k v -> Boolean
isEmpty t = case step t of
  Leaf -> true
  otherwise -> false

size :: forall k v. Map k v -> Int
size t = case step t of
  Leaf -> 0
  Bin m -> force m.size

weight :: forall k v. Map k v -> Int
weight t = size t + 1

-- | Create a map with one key/value pair
singleton :: forall k v. k -> v -> Map k v
singleton k v = bin k v empty empty

bin :: forall k v. k -> v -> Map k v -> Map k v -> Map k v
bin k v l r = Map $ pure $
  Bin { key: k
      , value: v
      , left: l
      , right: r
      , size: defer \_ -> size l + size r + 1
      }

cmp :: forall k. Ord k => k -> k -> Ordering
cmp = compare

-- | Lookup a value for the specified key
lookup :: forall k v. Ord k => k -> Map k v -> Maybe v
lookup k m = case step m of
  Leaf -> Nothing
  Bin t -> case cmp k t.key of
    EQ -> Just t.value
    LT -> lookup k t.left
    GT -> lookup k t.right

member :: forall k v. Ord k => k -> Map k v -> Boolean
member k = lookup k >>> isJust

instance lazyMap :: Z.Lazy (Map k v) where
  defer f = Map $ defer (step <<< f)

foldrWithKey :: forall k v b.
                Z.Lazy b =>
                (k -> v -> b -> b) ->
                b ->
                Map k v ->
                b
foldrWithKey f z t = go (step t) z
  where
    go Leaf acc = acc
    go (Bin b) acc =
      go (step b.left) (Z.defer \_ -> f b.key b.value (Z.defer \_ -> go (step b.right) acc))

toList :: forall k v. Map k v -> L.List (Tuple k v)
toList = foldrWithKey (\k v acc -> L.cons (Tuple k v) acc) L.nil

insertWith :: forall k v. Ord k =>
              (v -> v -> v) ->
              k ->
              v ->
              Map k v ->
              Map k v
insertWith f k v t = case (step t) of
  Leaf -> singleton k v
  Bin n -> case cmp k n.key of
    -- TODO: keep balanced
    EQ -> Map $ pure $ Bin $ n { value = f v n.value }
    LT -> Map $ pure $ Bin $ n { left = Z.defer \_ -> insertWith f k v n.left }
    GT -> Map $ pure $ Bin $ n { right = Z.defer \_ -> insertWith f k v n.right }
    -- LT -> balanceL n.key n.value (go f k v n.left) n.right
    -- GT -> balanceR n.key n.value n.left (go f k v n.right)

insert :: forall k v. Ord k => k -> v -> Map k v -> Map k v
insert = insertWith const

-- balanceL :: forall k v. k -> v -> Map k v -> Map k v -> Map k v
-- balanceL k v Leaf Leaf = singleton k v
-- balanceL k v l r = case r of
--   Leaf -> case l of
--     (Bin _ _ _ Leaf Leaf) -> bin k v l r
--     (Bin _ lk lv Leaf (Bin _ lrk lrv _ _)) ->
--       bin lrk lrv (singleton lk lv) (singleton k v)
--     (Bin _ lk lv ll@(Bin _ _ _ _ _) Leaf) -> bin lk lv ll (singleton k v)
--     (Bin ls lk lv ll@(Bin lls _ _ _ _) lr@(Bin lrs lrk lrv lrl lrr))
--       | lrs < ratio*lls ->
--           bin lk lv ll (bin k v lr empty)
--       | otherwise ->
--           bin lrk lrv (bin lk lv ll lrl) (bin k v lrr empty)

--   (Bin rs _ _ _ _) -> case l of
--     Leaf -> bin k v empty r
--     (Bin ls lk lv ll lr)
--       | ls > delta*rs ->
--         case (ll, lr) of
--           (Bin lls _ _ _ _, Bin lrs lrk lrv lrl lrr)
--             | lrs < ratio*lls ->
--               bin lk lv ll (bin k v lr r)
--             | otherwise ->
--                 bin lrk lrv (bin lk lv ll lrl) (bin k v lrr r)
--           (_, _) -> error "Failure in Data.Map.balanceL"
--       | otherwise -> Bin (1+ls+rs) k v l r

-- -- | Insert a key/value pair into a map
-- insert :: forall k v. Ord k => k -> v -> Map k v -> Map k v
-- insert = down Nil
--   where
--   cmp :: k -> k -> Ordering
--   cmp = compare

--   down :: List (TreeContext k v) -> k -> v -> Map k v -> Map k v
--   down ctx k v Leaf = up ctx (KickUp Leaf k v Leaf)
--   down ctx k v (Two left k1 v1 right) =
--     case cmp k k1 of
--       EQ -> fromZipper ctx (Two left k v right)
--       LT -> down (Cons (L k1 v1 right) ctx) k v left
--       _  -> down (Cons (R left k1 v1) ctx) k v right
--   down ctx k v (Three left k1 v1 mid k2 v2 right) =
--     case cmp k k1 of
--       EQ -> fromZipper ctx (Three left k v mid k2 v2 right)
--       c1 ->
--         case c1, cmp k k2 of
--           _ , EQ -> fromZipper ctx (Three left k1 v1 mid k v right)
--           LT, _  -> down (Cons (ThreeLeft k1 v1 mid k2 v2 right) ctx) k v left
--           GT, LT -> down (Cons (ThreeMiddle left k1 v1 k2 v2 right) ctx) k v mid
--           _ , _  -> down (Cons (ThreeRight left k1 v1 mid k2 v2) ctx) k v right

--   up :: List (TreeContext k v) -> KickUp k v -> Map k v
--   up Nil (KickUp left k v right) = Two left k v right
--   up (Cons x ctx) kup =
--     case x, kup of
--       L k1 v1 right, KickUp left k v mid -> fromZipper ctx (Three left k v mid k1 v1 right)
--       R left k1 v1, KickUp mid k v right -> fromZipper ctx (Three left k1 v1 mid k v right)

instance showMap :: (Show k, Show v) => Show (Map k v) where
  show m = "{" <> (joinWith ", " entryStrs) <> "}" where
    entryStrs = A.fromFoldable $ map (\(Tuple k v) -> show k <> ": " <> show v) $ toList m
