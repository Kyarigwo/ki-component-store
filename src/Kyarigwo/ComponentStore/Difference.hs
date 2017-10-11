{-

-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
module Kyarigwo.ComponentStore.Difference where

import Data.Proxy (Proxy(..))
import Data.Aeson (Value, Result(..), ToJSON(..), FromJSON, fromJSON)
import Data.Monoid ((<>))
import Data.Map.Strict (Map,
                        union,
                        mapMaybe,
                        intersectionWith,
                        foldrWithKey)
import qualified Data.Map.Strict as Map
import Control.Applicative ((<|>))
import Data.Text (Text)
import Data.Maybe (fromMaybe)

import Kyarigwo.Typename
import Kyarigwo.ComponentStore.HList
import Kyarigwo.ComponentStore.CStore
{-
## Difference

Finds the differences between two CStores, and can
apply these changes to a new event store.  Hence
we can save/load store changes.

A Difference is a map of to a Typename, identifying
the type, and a list of pair of EntityIds and Maybe Values.
If a Value is Nothing, the Value at EntityId for this
Type has been deleted.  Other wise it has been either
changed or added, and its new value is given.

-}
data Differences = Differences (Maybe EntityId) (Map (Text, EntityId) (Maybe Value))
  deriving (Eq, Show)

setEntityId :: EntityId -> EntityId -> Differences -> Differences
setEntityId newE oldE (Differences _ s) =
  if newE == oldE then Differences Nothing s
  else Differences (Just newE) s

instance Monoid Differences where
  mempty = Differences Nothing Map.empty
  mappend (Differences secondE secondD) (Differences firstE firstD) =
    Differences (secondE <|> firstE) (secondD `union` firstD)


difference :: Difference' (Tabled s) => CStore s -> CStore s -> Differences
difference (CStore newE newS) (CStore oldE oldS) =
  setEntityId newE oldE $ difference' newS oldS

infixl 6 |-|

(|-|) :: Difference' (Tabled s) => CStore s -> CStore s -> Differences
(|-|) = difference

applyDifferences :: Difference' (Tabled s) =>
  Differences -> CStore s -> CStore s
applyDifferences differences@(Differences me _) (CStore e s)
  = CStore (fromMaybe e me) $ applyDifferences' differences s

infix 6 |+|

(|+|) :: Difference' (Tabled s) =>
  Differences -> CStore s -> CStore s
(|+|) = applyDifferences

{-
 (x |-| y) |+| y = x
 x |-| x = mempty
 mempty |+| x = x
 d2 |+| (d1 |+| x) = (d2 <> d1) |+| x
-}

{-
Class can either take the difference between to items of s,
or apply a set of Differences to an s.
-}
class Difference' s where
  difference' :: s -> s -> Differences
  applyDifferences' :: Differences -> s -> s

-- instance Difference' (Tabled s) => Difference' (CStore s) where
--   difference (CStore s) (CStore s') = difference s s'
--   applyDifferences differences (CStore s) =
--     CStore (applyDifferences differences s)
{-
Difference between two pairs is the union of the
differences between each pair.
-}
instance (Difference' l, Difference' r) =>
  Difference' (l ⊠ r) where

  difference' (newl :*: newr) (oldl :*: oldr) =
    (difference' newl oldl) <> (difference' newr oldr)

  applyDifferences' differences (l :*: r) =
    applyDifferences' differences l ⊠ applyDifferences' differences r
{-
Differences between two tables?
-}
instance (Eq component,
         ToJSON component,
         FromJSON component,
         Typename component) =>
         Difference' (Table component) where

  difference' newTable oldTable =
      Differences Nothing $ tableDifference newTable oldTable

  applyDifferences' (Differences _ differences) table =
    let
      typeName = typename (Proxy :: Proxy component)
    in foldrWithKey (updateItem typeName) table differences

tableDifference :: forall component .
  (Eq component, ToJSON component, Typename component) =>
  Map EntityId component ->
  Map EntityId component ->
  Map (Text, EntityId) (Maybe Value)
tableDifference newTable oldTable =
  let
    added = fmap (Just . toJSON) $ newTable `Map.difference` oldTable
    deleted = Nothing <$ oldTable `Map.difference` newTable
    diff oldComponent newComponent =
      if oldComponent == newComponent then Nothing else Just newComponent
    changed = fmap (Just . toJSON)
      $ mapMaybe id
      $ intersectionWith diff oldTable newTable
    typeName = typename (Proxy :: Proxy component)
  in Map.mapKeys (typeName,) $ added `union` deleted `union `changed

updateItem ::
  FromJSON component =>
  Text ->
  (Text, EntityId) ->
  Maybe Value ->
  Map EntityId component ->
  Map EntityId component
updateItem typeName (tn', entityId) maybeValue oldTable =
  if typeName /= tn' then oldTable else
    case maybeValue of
      Nothing -> Map.delete entityId oldTable
      Just v -> changeValue entityId (fromJSON v) oldTable

changeValue ::
  EntityId ->
  Result c ->
  Table c -> Table c
changeValue _ (Error _) table = table
changeValue entityId (Success v) table =
  Map.insert entityId v table

{-
-}
