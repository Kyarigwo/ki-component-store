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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}
module Kyarigwo.ComponentStore
  (
    module Kyarigwo.ComponentStore.HList,
    module Kyarigwo.ComponentStore.CStore,
    module Kyarigwo.ComponentStore.Monad,
    module Kyarigwo.ComponentStore.Difference
  ) where

import Kyarigwo.ComponentStore.HList
import Kyarigwo.ComponentStore.CStore
import Kyarigwo.ComponentStore.Monad
import Kyarigwo.ComponentStore.Difference


{-
-}
