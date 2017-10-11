{-
-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Kyarigwo.ComponentStore.Monad where

import Data.Proxy (Proxy(..))

import Control.Monad.State.Strict (MonadState, State)
import qualified Control.Monad.State.Strict as S
import Data.Map.Strict (Map)

import Kyarigwo.ComponentStore.CStore
{-

## ComponentStore
A Monad for updating and storing changes.

Simply wraps a state containing a store.
-}
newtype ComponentStore store a =
  ComponentStore { unComponentStore :: (State (CStore store)) a}
  deriving (Functor, Applicative, Monad, MonadState (CStore store))
{-
### Execution
-}
run :: CStore store -> ComponentStore store a -> (a, CStore store)
run cstore actions =
  S.runState (unComponentStore actions) cstore

{-
### Monadic versions of the access/update functions
-}
select :: Contains row store => ComponentStore store (Map EntityId row)
select = select' <$> S.get

insert :: Contains row store => row -> EntityId -> ComponentStore store EntityId
insert row entityId =
  entityId <$ (S.modify $ insert' row entityId)

delete :: forall component store .
  component ∈ store
  => EntityId -> Proxy component -> ComponentStore store EntityId
delete entityId p =
  entityId <$ (S.modify $ delete' entityId p)

update :: forall store component .
  component ∈ store
  => EntityId -> (component -> component)
  -> ComponentStore store EntityId
update entityId f =
  entityId <$ (S.modify $ update' entityId f)

updateAll :: forall component store . component ∈ store
  => (component -> component) -> ComponentStore store ()
updateAll f =
  S.modify $ updateAll' f

fresh :: ComponentStore store EntityId
fresh = S.state fresh'
