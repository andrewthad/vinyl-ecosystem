{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Tagged.Functor where

import           Data.Proxy
import           Data.Tuple.TypeLevel (Fst, Snd)
import           Data.Vinyl.Core
import           Data.Vinyl.Functor   (Identity (..))
import           GHC.TypeLits

newtype TaggedFunctor (f :: b -> *) (x :: (a,b)) =
  TaggedFunctor { getTaggedFunctor :: f (Snd x) }
instance Eq (f (Snd x)) => Eq (TaggedFunctor f x) where
  TaggedFunctor a == TaggedFunctor b = a == b
instance Ord (f (Snd x)) => Ord (TaggedFunctor f x) where
  compare (TaggedFunctor a) (TaggedFunctor b) = compare a b
instance Show (f (Snd x)) => Show (TaggedFunctor f x) where
  show (TaggedFunctor f) = "TaggedFunctor (" ++ show f ++ ")"

showSymbolTaggedFunctor :: forall f x. (KnownSymbol (Fst x), Show (f (Snd x)))
  => TaggedFunctor f x -> String
showSymbolTaggedFunctor (TaggedFunctor f) =
  (symbolVal (Proxy :: Proxy (Fst x))) ++ ": " ++ show f

tagIdentity :: proxy k -> v -> TaggedFunctor Identity '(k,v)
tagIdentity _ v = TaggedFunctor (Identity v)

tagFunctor :: proxy k -> f v -> TaggedFunctor f '(k,v)
tagFunctor _ f = TaggedFunctor f

untagFunctor :: TaggedFunctor f x -> f (Snd x)
untagFunctor (TaggedFunctor f) = f

liftTaggedFunctor :: (f v -> a) -> TaggedFunctor f '(k,v) -> a
liftTaggedFunctor g (TaggedFunctor f) = g f

rtraverseTagged
  :: Applicative h
  => (forall x. f x -> h (g x))
  -> Rec (TaggedFunctor f) rs
  -> h (Rec (TaggedFunctor g) rs)
rtraverseTagged _ RNil      = pure RNil
rtraverseTagged f (TaggedFunctor x :& xs) =
  (:&) <$> fmap TaggedFunctor (f x) <*> rtraverseTagged f xs
{-# INLINABLE rtraverseTagged #-}

rtraverseIdentityTagged :: Applicative f
  => Rec (TaggedFunctor f) rs
  -> f (Rec (TaggedFunctor Identity) rs)
rtraverseIdentityTagged = rtraverseTagged (fmap Identity)
{-# INLINABLE rtraverseIdentityTagged #-}

