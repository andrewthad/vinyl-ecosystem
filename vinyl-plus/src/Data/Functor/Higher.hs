module Data.Functor.Higher 
  ( Functor1(..)
  , Monad1(..)
  , Comonad1(..)
  ) where

import Control.Monad

data Free f a = Return a | Nest (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Return a) = Return (f a)
  fmap f (Nest m) = Nest (fmap (fmap f) m)

instance Functor f => Applicative (Free f) where
  pure a = Return a
  Return f <*> Return a = Return (f a)
  Return f <*> Nest fa = Nest ((fmap.fmap) f fa)
  Nest   f <*> b = Nest $ fmap (<*> b) f

instance Functor f => Monad (Free f) where
  return = Return
  Return x >>= f = f x
  Nest m >>= f = Nest (fmap (>>= f) m)

class Functor1 m where
  -- should also imply Functor (f a) for any Functor a 
  -- because these are functors over functor categories, not just slice ones
  fmap1 :: (Functor f, Functor g) => (forall x. f x -> g x) -> m f y -> m g y

class Functor1 m => Monad1 m where
  return1 :: Functor f => f a -> m f a
  bind1 :: (Functor f, Functor g) => (forall a. f a -> m g a) -> m f b -> m g b
  default bind1 :: Functor (m g) => (Functor f, Functor g) => (forall a. f a -> m g a) -> m f b -> m g b
  bind1 f m = join1 (fmap1 f m)
  join1 :: Functor f => m (m f) a -> m f a

instance Functor1 Free where
  fmap1 f (Return x) = Return x
  fmap1 f (Nest m) = Nest (fmap (fmap1 f) (f m))

instance Monad1 Free where
  return1 x = Nest (fmap Return x)
  join1 (Return x) = Return x
  join1 (Nest m) = join (fmap join1 m)

data Cofree f a = a :< f (Cofree f a)

instance Functor f => Functor (Cofree f) where
  fmap f (a :< as) = f a :< fmap (fmap f) as

class Functor1 w => Comonad1 w where
  extract1 :: Functor f => w f a -> f a
  duplicate1 :: Functor f => w f a -> w (w f) a
  extend1 :: (Functor f, Functor g) => (forall a. w f a -> g a) -> w f b -> w g b
  default extend1 :: (Functor (w f), Functor f, Functor g) => (forall a. w f a -> g a) -> w f b -> w g b
  extend1 f = fmap1 f . duplicate1

instance Functor1 Cofree where
  fmap1 f (a :< as) = a :< fmap (fmap1 f) (f as)

instance Comonad1 Cofree where
  extract1 (a :< as) = fmap extract as
  duplicate1 x@(a :< as) = a :< (duplicate1 x :< fmap (fmap duplicate1 . duplicate) as)

instance Functor f => Comonad (Cofree f) where
  extract (a :< _) = a
  extend f x@(_ :< xs) = f x :< fmap (extend f) xs
  duplicate x@(_ :< xs) = x :< fmap duplicate xs

class Functor w => Comonad w where
  extract :: w a -> a

  duplicate :: w a -> w (w a)
  duplicate = extend id

  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

