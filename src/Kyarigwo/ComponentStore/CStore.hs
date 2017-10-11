{-

-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeApplications           #-}

module Kyarigwo.ComponentStore.CStore where

import Data.Proxy (Proxy(..))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lens.Micro (Lens', (&), (%~), (^.))

import Kyarigwo.ComponentStore.HList
{-

## ComponentStore
The main point of this library.  Uses the heterogeneous lists from above both
as leaf stores, and also as rows in insert/select queries.

### EntityId
Primary key of the tables in the ComponentStore.
-}
newtype EntityId = EntityId Int
  deriving (Eq, Show, Ord, Enum)

initial :: EntityId
initial = EntityId 0
{-
### Table component
A simple alias for EntityId tables.
-}
type Table component = Map EntityId component

{-
### element of
A constraint which represents the assertion that store type store
contains items of type component.
-}
type component ∈ store = Has (Table component) (Tabled store)

{-
## CStore store
The point of all this.  Type CStore store is a component store for
components where component ∈ store

-}
data CStore store = CStore EntityId (Tabled store)

instance Eq (Tabled store) => Eq (CStore store) where
  (CStore e s) == (CStore e' s') = s == s' && e == e'

instance Show (Tabled store) => Show (CStore store) where
  show (CStore e s) = "CStore " ++ show e ++ " " ++ show s

_Store :: Lens' (CStore store) (Tabled store)
_Store f (CStore e store) = CStore e <$> f store

_Entity :: Lens' (CStore store) EntityId
_Entity f (CStore e store) = (`CStore` store) <$> f e

type family Tabled store :: * where
  Tabled (l ⊠ r) = (Tabled l) ⊠ (Tabled r)
  Tabled a = Table a

{-
### empty
Creates a new empty instance of the store CStore store.

First, a type class function signature.
-}
class Empty' store where
  empty' :: store
{-
If the store only has a single table, create it.
-}
instance Empty' (Table component) where
  empty' = Map.empty
{-
If the store contains a left and right branch, create both
and store the two results in a left and right branch.
-}
instance (Empty' l, Empty' r) => Empty' (l ⊠ r) where
  empty' = empty' ⊠ empty'
{-
Finally, the empty function just uses empty' to create an empty
set of tables, and wraps a CStore constructor around it.
-}
empty :: Empty' (Tabled store) => CStore store
empty = CStore initial empty'

_Table :: component ∈ store => Lens' (CStore store) (Table component)
_Table = _Store . ele
{-
## Contains
This is the main interface to the store.  Allows row based insert/selects/deletes.

Note that the select'' operation does not return a Map EntityId row, but
rather a IJMap (inner join map), which is a GADT. We will explain why this
is needed later.
-}
class Contains row s where
  insert' :: row -> EntityId -> CStore s -> CStore s
  select'' :: CStore s -> IJMap row
{-
For the empty row, insert' does nothing, and select'' returns an empty
IJMap. We can't just return an empty (Map EntityId, N), we need to
distinguish between this case, when we are starting the join, and the
case when the join is really empty.
-}
instance Contains N s where
  insert' _ _ cstore = cstore
  select'' _ = EMap
{-
For a non-empty row, where component is in the store,
and the remainder of the row is also in the store.
-}
instance (component ∈ store, Contains remainder store) =>
         Contains (component⊠remainder) store where
{-
insert' just inserts the component at entityId into the
table, and recurs.
-}
  insert' (component :*: remainder) entityId cstore =
    let
      cstore' = cstore & _Table %~ Map.insert entityId component
    in insert' remainder entityId cstore'
{-
select'' does an innerJoin between this table and the result of
selecting on the remainder.
-}
  select'' cstore =
    innerJoin (cstore^._Table) (select'' cstore)
{-
The definition of IJMap.  EMap is the base case, IJMap is the
non-empty row case
-}
data IJMap row where
  EMap :: IJMap N
  IJMap :: Map EntityId row -> IJMap row
{-
The inner join function.
-}
innerJoin :: Table component -> IJMap row -> IJMap (component ⊠ row)
{-
In the case we are joining to an empty IJmap, we just convert the component
into a row, and wrap the result.  However, if IJMap was not a GADT the
type checker would not know that row was N, and hence that component ⊠ N was
the correct return type.
-}
innerJoin table EMap = IJMap (fmap (⊠N) table)
{-
In this case we just use Data.Map's intersectionWith function, and join the
component to the row.
-}
innerJoin table (IJMap table') =
  IJMap $ Map.intersectionWith (⊠) table table'
{-
### select'
The select' function unwraps select''s IJMap.
-}
select' :: forall row store . Contains row store => CStore store -> Map EntityId row
select' cstore =
  case select'' cstore of
    EMap -> Map.empty
    IJMap table -> table
{-
### delete'
Deletes a component at a certain entityId from the store.
-}
delete' :: forall component store . component ∈ store
  => EntityId -> Proxy component -> CStore store -> CStore store
delete' entityId _ cstore =
  cstore & _Table %~ Map.delete @EntityId @component entityId
{-
### update'
Updates a component of an EntityId
-}
update' :: forall component store . component ∈ store
  => EntityId -> (component -> component) -> CStore store -> CStore store
update' entityId f cstore =
  cstore & _Table %~ Map.adjust f entityId
{-
### UpdateAll'
Applies an update to all components of a certain type
-}
updateAll' :: forall component store . component ∈ store
  => (component -> component) -> CStore store -> CStore store
updateAll' f cstore = cstore & _Table %~ fmap f

fresh' :: CStore store -> (EntityId, CStore store)
fresh' (CStore e store) = (e, CStore (succ e) store)
{-
-}
