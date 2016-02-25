module Data.TypeMap
  ( TypeMap
  , empty
  , insert
  , lookup
  , lookup'
  ) where

import Prelude hiding (lookup)
import Data.Typeable
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)

newtype TypeMap (f :: * -> *) = 
  TypeMap { getTypeMap :: HashMap TypeRep Any }

empty :: TypeMap f
empty = TypeMap HashMap.empty

insert :: Typeable a => f a -> TypeMap f -> TypeMap f
insert fa (TypeMap m) = 
  TypeMap (HashMap.insert (typeRep fa) (toAny2 fa) m)

lookup :: forall a f . Typeable a => TypeMap f -> Maybe (f a)
lookup = lookup' (Proxy :: Proxy a)

lookup' :: Typeable a => proxy a -> TypeMap f -> Maybe (f a)
lookup' p (TypeMap m) = fmap fromAny2 (HashMap.lookup (typeRep p) m)

map :: (forall a. f a -> g a) -> TypeMap f -> TypeMap g
map f (TypeMap m) = TypeMap (HashMap.map (toAny2 . f . fromAny2) m)

toAny2 :: f a -> Any
toAny2 = unsafeCoerce

fromAny2 :: Any -> f a
fromAny2 = unsafeCoerce

toAny :: a -> Any
toAny = unsafeCoerce

fromAny :: Any -> a
fromAny = unsafeCoerce

-- module Data.TypeMap 
--   ( TypeMap
--   , insert
--   , lookup
--   , lookup'
--   ) where
-- 
-- import Prelude hiding (lookup)
-- import Data.Typeable
-- import qualified Data.HashMap.Strict as HashMap
-- import Data.HashMap.Strict (HashMap)
-- import GHC.Prim (Any)
-- import Unsafe.Coerce (unsafeCoerce)
-- 
-- newtype TypeMap = TypeMap { getTypeMap :: HashMap TypeRep Any }
-- 
-- insert :: Typeable a => a -> TypeMap -> TypeMap
-- insert a (TypeMap m) = 
--   TypeMap (HashMap.insert (typeOf a) (unsafeCoerce a) m)
-- 
-- lookup :: forall a. Typeable a => TypeMap -> Maybe a
-- lookup = lookup' (Proxy :: Proxy a)
-- 
-- lookup' :: Typeable a => proxy a -> TypeMap -> Maybe a
-- lookup' p (TypeMap m) = fmap unsafeCoerce (HashMap.lookup (typeRep p) m)
-- 
-- -- modify :: Typeable a => (a -> a) -> TypeMap -> TypeMap
-- -- modify 

