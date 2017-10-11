{-
# Tutorial
[Back](../../README.md)

This is written as a test module, so that the examples should be
automatically tested as well.
-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE OverloadedStrings       #-}
module Kyarigwo.TutorialTest where

import GHC.Generics (Generic)
import Data.Function ((&))
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as A
import Control.Monad (void)
import qualified Data.Map.Strict as Map

import Test.Tasty.Hspec

import Kyarigwo.ComponentStore
import Kyarigwo.Typename

{-
To start with, we introduce the basic pair type we use, called
```⊠```.   This is a simple product type, and so we can use it
to create trees of types, such as
-}
type IntAndString = Int ⊠ String
{-
We can also create heterogeneous list of types, with ```⊠``` as
the 'cons' and the type ```N``` as the empty list.
-}
type StringAndIntTuple = String ⊠ Int ⊠ N
{-
We can create elements of these types, either with the constructor
```:*:```, or the function ```⊠```.
-}
intAndString :: IntAndString
intAndString = 42 :*: "Hello World"

stringAndIntTuple ::  StringAndIntTuple
stringAndIntTuple = "Not yet the end" ⊠ 137 ⊠ N
{-
These can be taken apart by pattern matching.  But more interesting,
we can also access elements by type with the ```get``` function.
-}
spec_BasicGet :: Spec
spec_BasicGet =
  describe "getting" $ do
    it "the item to get is indicated by the return type inferred"
       (get @String intAndString `shouldBe` "Hello World")
    it "and does not depend on the shape of the list"
       (get @String stringAndIntTuple `shouldBe` "Not yet the end")
    it "does need to know the required type tho'"
        (get stringAndIntTuple `shouldBe` (137 :: Int))
    it "we can use explicit type applications here"
        (get @Int intAndString `shouldBe` 42)
{-

The interesting thing about this is that it is type safe, that is, if
we try to get a type not contained in the list, we get a compile type
error.

So if the following is commented in, a type error will be raised.

-}
-- type StringAndString = String ⊠ String

-- stringAndString :: StringAndString
-- stringAndString = "Hello World" ⊠ "Goodbye World"

-- spec_GetTypeChecking :: Spec
-- spec_GetTypeChecking =
--   describe "get type checking" $ do
--     it "doesn't allow you to try to get a value not contained in the type"
--       (get intAndString `shouldBe` 'a')
--     it "doesn't allow more then of one type in a type"
--       (get stringAndString `shouldBe` "HelloWorld")
{-
Similarly we don't allow multiple instances of the same type in a type, since
we don't know which type to pull out with ```get```

There is also a function, ```set``` which can set a value in a type, and
has the same type checking as ```get```, however it is not used as much.

Now the main event.

First, a arbitrary data type.  This is like a 'nothing up my sleeves'
type.
-}
data Foo = Foo Int
  deriving (Eq, Show, Generic, ToJSON, FromJSON, Typename )
{-
A ```Store``` containing Strings, Bools and Foos.  So you can put arbitrary
types in a store, with nothing special needing to be done to them.  The
types stored are indicated with a hetlist type.
-}
type Contents = String⊠Bool⊠Foo
{-
Create a empty store with ```empty```.  The return type is inferred
-}
store :: CStore Contents
store =
  empty
{-
Insert values with ```insert'```.  We pass a hetlist, along with
the EntityId to store it under.  ```insert'``` will overwrite any
existing values.
-}
    & insert' @(String⊠Bool⊠N) ("Hello World"⊠False⊠N) (EntityId 0)
    & insert' @(Foo ⊠ String ⊠ N) (Foo 42 ⊠ "Goodby World" ⊠ N) (EntityId 1)
    & insert' @(Bool ⊠ Foo ⊠ String ⊠ N) (True ⊠ Foo 137 ⊠ "Not yet the end" ⊠ N ) (EntityId 2)

emptyStore :: CStore Contents
emptyStore =
  empty
{-
Also, this is type safe, it would be a compile time error if we uncomented the
following.
-}
--    & insert' ((137::Int)⊠N) (EntityId 2)
{-
Retrieve values stored with ```select'```.  This returns a
```Map EntityId hetlist```, the type of *hetlist* inferred.
-}
spec_CStoreSelect :: Spec
spec_CStoreSelect =
  describe "select' function" $ do
    it "can select' strings" $
      (Map.toList $ select' @(String⊠N) store) `shouldBe`
          [(EntityId 0, "Hello World"⊠N),
           (EntityId 1, "Goodby World"⊠N),
           (EntityId 2, "Not yet the end"⊠N)]
    it "can select' Bools" $
      (Map.toList $ select' store) `shouldBe`
          [(EntityId 0, False⊠N),
          (EntityId 2, True⊠N)]
    it "can select' Foos" $
      (Map.toList $ select' store) `shouldBe`
          [(EntityId 1, (Foo 42)⊠N), (EntityId 2, (Foo 137)⊠N)]
    it "can select' Foos and Bools" $
      (Map.toList $ select' store) `shouldBe`
               [(EntityId 2, (Foo 137 ⊠ True ⊠ N))]
    it "can select' Foos and Bools" $
      (Map.toList $ select' @(Foo⊠Bool⊠N) store) `shouldBe`
               [(EntityId 2, (Foo 137 ⊠ True ⊠ N))]
{-
Again, this is type safe.

Component store Monad
-}

test :: (Eq a, Show a)
  => a -> ComponentStore Contents a -> Expectation
test target m =
  (fst $ run emptyStore m) `shouldBe` target


spec_ComponentStore :: Spec
spec_ComponentStore =
  describe "ComponentStore" $ do
    it "can insert" $ test [(EntityId 0, "Hello World"⊠N)] $ do
      void $ insert @(String⊠N) ("Hello World"⊠N) (EntityId 0)
      Map.toList <$> select @(String⊠N)


spec_Difference :: Spec
spec_Difference =
  describe "Difference" $ do
    it "difference of two empties is empty" $
      emptyStore |-| emptyStore `shouldBe` mempty
    it "difference of two non-empty is empty" $
      store |-| store `shouldBe` mempty
    it "difference of non-empty and empty is" $
      store |-| emptyStore `shouldBe`
        Differences Nothing (Map.fromList [
                         (("String", EntityId 0), (Just $ A.String "Hello World")),
                         (("Bool", EntityId 0), (Just $ A.Bool False)),

                         (("Foo", EntityId 1), (Just $ A.Number 42)),
                         (("String", EntityId 1), (Just $ A.String "Goodby World")),

                         (("Bool", EntityId 2), (Just $ A.Bool True)),
                         (("Foo", EntityId 2), (Just $ A.Number 137)),
                         (("String", EntityId 2), (Just $ A.String "Not yet the end"))
                          ])
    it "difference of empty and non-empty" $
      emptyStore |-| store `shouldBe`
          Differences Nothing (Map.fromList [
                         (("String", EntityId 0), Nothing),
                         (("Bool", EntityId 0), Nothing),

                         (("Foo", EntityId 1), Nothing),
                         (("String", EntityId 1), Nothing),

                         (("Bool", EntityId 2), Nothing),
                         (("Foo", EntityId 2), Nothing),
                         (("String", EntityId 2), Nothing)
                          ])

spec_ApplyDifferences :: Spec
spec_ApplyDifferences =
  describe "Apply Differences" $ do
     it "has (x |-| y) |+| y = x" $
       ((store |-| emptyStore) |+| emptyStore) `shouldBe` store
{-

-}
