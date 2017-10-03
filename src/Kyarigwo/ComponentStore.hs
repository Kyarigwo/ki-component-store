{-
# Component Store
[Back](../../README.md)

Types and functions to implement an Entity/Component store.

Some initial stuff.
-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs                      #-}
module Kyarigwo.ComponentStore where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import Data.Proxy (Proxy(..))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
{-
## Basic product type
This type is used for products, in two ways.  First, we use it for
representing rows and n-tuples and second, it is used for the branches
of the tree structure in which the components are stored.

-}
infixr 2 ⊠
infixr 2 :*:

data l ⊠ r = l :*: r

(⊠) :: l -> r -> l ⊠ r
l ⊠ r = l :*: r
{-

Types are like ```Int ⊠ String```, and values are constructed via either the
```:*:``` constructor, or the function ```⊠```

-}
instance (Show l, Show r) => Show (l ⊠ r) where
  show (l :*: r) = "(" ++ show l ++" ⊠ " ++ show r ++ ")"

instance (Eq l, Eq r) => Eq (l ⊠ r) where
  (l1 :*: r1) == (l2 :*: r2) = l1 == l2 && r1 == r2

{-
### N type
If we use the ```⊠``` type to create a heterogeneous list, the
```N``` type is used as the end marker.

-}
data N = N
  deriving (Show, Eq)


{-
## Internal implementation

This code implements a type level search through a tree of products (*haystack*)
for a *needle* typed element which in contained inside, with a compile time error
raised if either the *needle* type is not present, or if there are multiple instances
of the *needle* type found.

### Crumbs
Values of this type represent a path through the *haystack* to the
*needle* type.  We use DataKinds to raise this to the type level.

-}
data Crumbs = Here | L Crumbs | R Crumbs

{-
### Res
The result of a type directed search.  We either found the *needle*, and hence
will have a path to it, failed to find it, or found multiple instances.
-}
data Res = Found Crumbs | NotFound | Ambiguous

{-
### Sparkly Magical Haskell Type stuff. [*](https://aphyr.com/posts/342-typing-the-technical-interview)
The following is a type level function to search, at compile time,
for the *needle* in the *haystack*.

Look for a *needle* in a binary *haystack*
-}
type family Search needle haystack :: Res where
{-
If the *haystack* is only the *needle* we have found it here.
-}
  Search needle needle = 'Found 'Here
{-
If the *haystack* is a product type, search left and right,
and combine the results.
-}
  Search needle (l ⊠ r) = Combine (Search needle l) (Search needle r)
{-
Otherwise we did not find our goal
-}
  Search needle notNeedle = 'NotFound
{-

Combine recursive searches down the left and right branch into
a single result.
-}
type family Combine left right :: Res where
{-
If we found the *needle* down both branches, the
*needle* occurs twice, so this is Ambiguous
-}
  Combine ('Found a) ('Found b) = 'Ambiguous
  Combine 'Ambiguous b = 'Ambiguous
  Combine a 'Ambiguous = 'Ambiguous
{-
If we found it down the left branch, add that crumb to the
path, and the same for the right branch.
-}
  Combine ('Found a) b = 'Found ('L a)
  Combine a ('Found b) = 'Found ('R b)
{-
Other wise, we did not find the *needle*
-}
  Combine a b = 'NotFound

{-
### Has'
Expresses the constraint that *haystack* has *needle*.  Uses the
above type function.  Also provides the basic getter and setters,
as well as custom type errors if the *needle* is not found or
ambiguous.

-}
class Has' (res :: Res) haystack needle where
  get' :: Proxy res -> haystack -> needle
  set' :: Proxy res -> haystack -> needle -> haystack

{-
```Has'``` has instances for the case where we have a ```Found``` ```Res```,
following the crumbs to find the *needle*.

Either is is ```'Here```
-}
instance Has' ('Found 'Here) needle needle where
  get' _ needle = needle
  set' _ _ needle = needle

{-
or down the left branch
-}
instance Has' ('Found p) l needle => Has' ('Found ('L p)) (l ⊠ r) needle where
  get' _ (l :*: _) = get' (Proxy :: Proxy ('Found p)) l
  set' _ (l :*: r) needle = set' (Proxy :: Proxy ('Found p)) l needle ⊠ r

{-
or down the right branch
-}
instance Has' ('Found p) r needle  => Has' ('Found  ('R p)) (l ⊠ r) needle where
  get' _ (_ :*: r) = get' (Proxy :: Proxy ('Found p)) r
  set' _ (l :*: r) needle = l ⊠ set' (Proxy :: Proxy ('Found p)) r needle

{-
#### Type Errors
If we don't find the *needle* we will have a compile time error.  By default,
this would be an 'implementation of ```Has'``` not found' error.  But by using
custom type errors, we can make this nicer.

-}
instance TypeError ('Text "Type " ':<>: 'ShowType s
                   ':$$: 'Text "Does not contain " ':<>: 'ShowType c ':<>: 'Text " types")
  => Has' 'NotFound s c where
  get' = error "Not defined"
  set' = error "Not defined"

instance TypeError ('Text "Type " ':<>: 'ShowType s
                    ':<>: 'Text "Contains multiple instances of " ':<>: 'ShowType c)
         => Has' 'Ambiguous s c where
  get' = error "Not defined"
  set' = error "Not defined"

{-
## User visible code
Finally, we get the the user api for the above code.

### Has
First, a nice type synonym for the Has' contraint
-}
type Has needle haystack = Has' (Search needle haystack) haystack needle

{-
### get
Then a get function
-}
get :: forall needle haystack . Has needle haystack => haystack -> needle
get = get' (Proxy :: Proxy (Search needle haystack))

{-
### set
and a set function
-}
set :: forall needle haystack . Has needle haystack => haystack -> needle -> haystack
set = set' (Proxy :: Proxy (Search needle haystack))
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
data CStore store = CStore (Tabled store)

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
empty = CStore empty'
{-
### getTable
gets the table in the store containing components of type
component.
-}
getTable :: component ∈ store => CStore store -> Table component
getTable (CStore store) = get store
{-
### setTable
sets the table in the store containing components of type
component.
-}
setTable :: component ∈ store => CStore store -> Table component -> CStore store
setTable (CStore store) table = CStore (set store table)
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
      table' = Map.insert entityId component (getTable cstore)
      cstore' = setTable cstore table'
    in insert' remainder entityId cstore'
{-
select'' does an innerJoin between this table and the result of
selecting on the remainder.
-}
  select'' cstore =
    innerJoin (getTable cstore) (select'' cstore)
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
  let
    table = getTable cstore :: Table component
    table' = Map.delete entityId table
  in setTable cstore table'
