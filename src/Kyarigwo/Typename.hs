{-

-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
module Kyarigwo.Typename where

import Data.Word (Word64)
import Data.Text (Text, pack)
import Data.Proxy
import GHC.Generics hiding ((:*:))


class Typename a where
  typename :: Proxy a -> Text

  default typename ::(Generic a, GTypeName (Rep a)) => Proxy a -> Text
  typename _ = gtypename (from (undefined :: a))

class GTypeName f where
  gtypename :: f a -> Text

instance (Datatype c) => GTypeName (M1 i c f) where
  gtypename m = pack $ datatypeName m

instance Typename Bool

instance Typename Char where
  typename _ = "Char"

instance Typename Text where
  typename _ = "Text"

instance Typename Int where
  typename _ = "Int"

instance Typename Word64 where
  typename _ = "Word64"

instance Typename [Char] where
  typename _ = "String"
