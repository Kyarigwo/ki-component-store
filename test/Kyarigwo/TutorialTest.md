# Tutorial
[Back](../../README.md)

This is written as a test module, so that the examples should be
automatically tested as well.
```haskell
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE TypeApplications        #-}
module Kyarigwo.TutorialTest where

import Data.Function ((&))
import qualified Data.Map.Strict as Map

import Test.Tasty.Hspec

import Kyarigwo.ComponentStore

```
To start with, we introduce the basic pair type we use, called
```⊠```.   This is a simple product type, and so we can use it
to create trees of types, such as
```haskell
type IntAndString = Int ⊠ String
```
We can also create heterogeneous list of types, with ```⊠``` as
the 'cons' and the type ```N``` as the empty list.
```haskell
type StringAndIntTuple = String ⊠ Int ⊠ N
```
We can create elements of these types, either with the constructor
```:*:```, or the function ```⊠```.
```haskell
intAndString :: IntAndString
intAndString = 42 :*: "Hello World"

stringAndIntTuple ::  StringAndIntTuple
stringAndIntTuple = "Not yet the end" ⊠ 137 ⊠ N
```
These can be taken apart by pattern matching.  But more interesting,
we can also access elements by type with the ```get``` function.
```haskell
spec_BasicGet :: Spec
spec_BasicGet =
  describe "getting" $ do
    it "the item to get is indicated by the return type inferred"
       (get intAndString `shouldBe` "Hello World")
    it "and does not depend on the shape of the list"
       (get stringAndIntTuple `shouldBe` "Not yet the end")
    it "does need to know the required type tho'"
        (get stringAndIntTuple `shouldBe` (137 :: Int))
    it "we can use explicit type applications here"
        (get @Int intAndString `shouldBe` 42)
```

The interesting thing about this is that it is type safe, that is, if
we try to get a type not contained in the list, we get a compile type
error.

So if the following is commented in, a type error will be raised.

```haskell
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
```
Similarly we don't allow multiple instances of the same type in a type, since
we don't know which type to pull out with ```get```

There is also a function, ```set``` which can set a value in a type, and
has the same type checking as ```get```, however it is not used as much.

```haskell
data Foo = Foo Int
  deriving (Eq, Show)

type Store = CStore (String⊠Bool⊠Foo)

store :: Store
store =
  empty
    & insert' ("Hello World"⊠False⊠N) (EntityId 0)
    & insert' (Foo 42 ⊠ "Goodby World" ⊠ N) (EntityId 1)
    & insert' (True ⊠ Foo 137 ⊠ "Not yet the end" ⊠ N ) (EntityId 2)
--    & insert' ((137::Int)⊠N) (EntityId 2)

spec_CStoreSelect :: Spec
spec_CStoreSelect =
  describe "select' function" $ do
    it "can select' strings" $
      (Map.toList $ select' store) `shouldBe`
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


```

