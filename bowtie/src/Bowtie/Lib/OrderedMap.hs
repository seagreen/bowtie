-- | Copied (and then subsequently modified) from:
-- http://hackage.haskell.org/package/ordered-containers
-- a BSD-3 library by Daniel Wagner.
module Bowtie.Lib.OrderedMap
  ( OrderedMap
  , empty
  , singleton
  , insert
  , append
  , lookup
  , delete
  , fromList
  , toList
  , keys
  , elems
  ) where

import Bowtie.Lib.Prelude hiding (empty, toList)
import Data.Data (Data)
import Data.Map (Map)
import Prelude (succ)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map

-- | Note that entries show up twice when printed by 'Show',
-- but there's no way around this if we want to obey the show/read law.
data OrderedMap k v
  = OrderedMap
      !(HashMap k (Natural, v))
      !(Map Natural (k, v))
      -- ^ Using Map for maxViewWithKey
  deriving stock (Eq, Show, Functor, Data)

-- | NOTE: Not used right now?
instance Foldable (OrderedMap k) where
  foldMap :: Monoid m => (a -> m) -> OrderedMap k a -> m
  foldMap f (OrderedMap _ ivs) =
    foldMap (f . snd) ivs

instance (Eq k, Hashable k) => Traversable (OrderedMap k) where
  traverse :: Applicative f => (a -> f b) -> OrderedMap k a -> f (OrderedMap k b)
  traverse f (OrderedMap _ ivs) =
    fromKV <$> traverse (\(k,v) -> (,) k <$> f v) ivs

-- | Internal for @Traversable@.
fromKV :: forall k v. (Eq k, Hashable k) => Map Natural (k, v) -> OrderedMap k v
fromKV ivs =
  OrderedMap kvs ivs
  where
    kvs :: HashMap k (Natural, v)
    kvs =
      HashMap.fromList [(k,(t,v)) | (t,(k,v)) <- Map.toList ivs]

empty :: (Eq k, Hashable k) => OrderedMap k v
empty =
  OrderedMap mempty mempty

singleton
  :: (Eq k, Hashable k)
  => k
  -> v
  -> OrderedMap k v
singleton k v =
  OrderedMap (HashMap.singleton k (0, v)) (Map.singleton 0 (k, v))

insert
  :: (Eq k, Hashable k)
  => k
  -> v
  -> OrderedMap k v
  -> Maybe (OrderedMap k v)
insert k v o@(OrderedMap kvs nvs) =
  case lookup k o of
    Just _ ->
      Nothing

    Nothing ->
      Just $
        OrderedMap
          (HashMap.insert k (n, v) kvs)
          (Map.insert n (k, v) nvs)
  where
    n :: Natural
    n =
      nextHigherTag nvs

append :: (Eq k, Hashable k) => OrderedMap k v -> OrderedMap k v -> Either k (OrderedMap k v)
append o1 o2 =
  foldM insertEither o1 (toList o2)

lookup :: (Eq k, Hashable k) => k -> OrderedMap k v -> Maybe v
lookup k (OrderedMap hm _) =
  fmap snd (HashMap.lookup k hm)

delete :: (Eq k, Hashable k) => k -> OrderedMap k v -> OrderedMap k v
delete k o@(OrderedMap kvs ivs) =
  case HashMap.lookup k kvs of
    Nothing ->
      o

    Just (n, _) ->
      OrderedMap (HashMap.delete k kvs) (Map.delete n ivs)

fromList :: (Eq k, Hashable k) => [(k, v)] -> Either k (OrderedMap k v)
fromList =
  foldM insertEither empty

toList :: OrderedMap k v -> [(k, v)]
toList (OrderedMap _ ivs) =
  fmap snd (Map.toAscList ivs)

keys :: OrderedMap k v -> [k]
keys =
  fmap fst . toList

elems :: OrderedMap k v -> [v]
elems =
  fmap snd . toList

-- | Internal.
nextHigherTag :: Map Natural v -> Natural
nextHigherTag =
  maybe 0 succ . maxTag
  where
    maxTag :: Map k v -> Maybe k
    maxTag =
      fmap (fst . fst) . Map.maxViewWithKey

insertEither :: (Eq k, Hashable k) => OrderedMap k v -> (k, v) -> Either k (OrderedMap k v)
insertEither o (k, v) =
  maybe (Left k) Right (insert k v o)
