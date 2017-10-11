{-

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
{-# LANGUAGE RankNTypes                 #-}
module Kyarigwo.ComponentStore.HList where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import Data.Proxy (Proxy(..))

import Lens.Micro (Lens')
import Lens.Micro.Extras (view)
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
above type function.  Also provides a lens, ```ele'```
as well as custom type errors if the *needle* is not found or
ambiguous.

-}
class Has' (res :: Res) haystack needle where
  ele' :: Proxy res -> Lens' haystack needle
{-
```Has'``` has instances for the case where we have a ```Found``` ```Res```,
following the crumbs to find the *needle*.

Either is is ```'Here```
-}
instance Has' ('Found 'Here) needle needle where
  ele' _ f s = f s
{-
or down the left branch
-}
instance Has' ('Found p) l needle => Has' ('Found ('L p)) (l ⊠ r) needle where
  ele' _ f (l :*: r) = ( ⊠ r) <$> ele' (Proxy :: Proxy ('Found p)) f l
{-
or down the right branch
-}
instance Has' ('Found p) r needle  => Has' ('Found  ('R p)) (l ⊠ r) needle where
  ele' _ f (l :*: r) = (l ⊠) <$> ele' (Proxy :: Proxy ('Found p)) f r
{-
#### Type Errors
If we don't find the *needle* we will have a compile time error.  By default,
this would be an 'implementation of ```Has'``` not found' error.  But by using
custom type errors, we can make this nicer.

-}
instance TypeError ('Text "Type " ':<>: 'ShowType s
                   ':$$: 'Text "Does not contain " ':<>: 'ShowType c ':<>: 'Text " types")
  => Has' 'NotFound s c where
  ele' = error "Not defined"

instance TypeError ('Text "Type " ':<>: 'ShowType s
                    ':<>: 'Text "Contains multiple instances of " ':<>: 'ShowType c)
         => Has' 'Ambiguous s c where
  ele' = error "Not defined"

{-
## User visible code
Finally, we get the the user api for the above code.

### Has
First, a nice type synonym for the Has' contraint
-}
type Has needle haystack = Has' (Search needle haystack) haystack needle
{-
### ele
Lens to get elements
-}
ele :: forall needle haystack . Has needle haystack => Lens' haystack needle
ele = ele' (Proxy :: Proxy (Search needle haystack))
{-
### get
Then a get function
-}
get :: forall needle haystack . Has needle haystack => haystack -> needle
get = view ele

