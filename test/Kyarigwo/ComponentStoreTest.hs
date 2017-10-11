
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Kyarigwo.ComponentStoreTest where

import Data.Function ((&))
import Data.Monoid ((<>))

import Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Lens.Micro ((.~))

import Kyarigwo.ComponentStore
import Kyarigwo.Typename


data Foo = Foo Int
  deriving (Eq, Show, Generic, ToJSON, FromJSON, Typename)

foo :: Gen Foo
foo = Foo <$> Gen.int (Range.constantFrom 0 (-10) 10)

data Bar = Bar String
  deriving (Eq, Show, Generic, ToJSON, FromJSON, Typename)

bar :: Gen Bar
bar = Bar <$> Gen.string (Range.constantFrom 0 0 10) Gen.ascii

data Baz = Baz Bool
  deriving (Eq, Show, Generic, ToJSON, FromJSON, Typename)

baz :: Gen Baz
baz  = Baz <$> Gen.bool

entityId :: Gen EntityId
entityId = EntityId <$> Gen.int (Range.constantFrom 0 0 5)

ofT :: Gen a -> Gen [(EntityId, a)]
ofT g = Gen.list (Range.constantFrom 0 0 4) $ (,) <$> entityId <*> g

type Contents = Foo ⊠ Bar ⊠ Baz

insertTuple :: a ∈ s => (EntityId, a) -> CStore s -> CStore s
insertTuple (eid, a) s = insert' (a ⊠ N) eid s

cStore :: Gen (CStore Contents)
cStore = do
  let p = flip (foldr insertTuple) 
  foos <- ofT foo
  bars <- ofT bar
  bazs <- ofT baz
  e <- entityId
  pure $
    (empty :: CStore Contents)
     & p foos
     & p bars
     & p bazs
     & _Entity .~ e

diff :: Gen Differences
diff = Gen.choice [pure mempty, (|-|) <$> cStore <*> cStore]

hprop_Difference_leftId :: Property
hprop_Difference_leftId = property $ do
  d <- forAll diff
  mempty <> d === d

hprop_Difference_rightId :: Property
hprop_Difference_rightId = property $ do
  d <- forAll diff
  d <> mempty === d

hprop_Difference_assoc :: Property
hprop_Difference_assoc = property $ do
  d1 <- forAll diff
  d2 <- forAll diff
  d3 <- forAll diff
  (d1 <> d2) <> d3 === d1 <> (d2 <> d3)

hprop_ApplyDiff :: Property
hprop_ApplyDiff = property $ do
  x <- forAll cStore
  y <- forAll cStore
  (y |-| x) |+| x === y

hprop_DiffId :: Property
hprop_DiffId = property $ do
  x <- forAll cStore
  x |-| x === mempty

hprop_ApplyAction :: Property
hprop_ApplyAction = property $ do
  d1 <- forAll diff
  d2 <- forAll diff
  x <- forAll cStore
  d2 |+| (d1 |+| x) === (d2 <> d1) |+| x
