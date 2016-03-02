{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Vinyl.Plus.Functor where

import           Data.Functor.Contravariant
import           Data.Vinyl.Core
import           Data.Vinyl.Plus.TypeLevel
import           Data.Vinyl.TypeLevel
import           Data.Vinyl.Types

newtype FunctorRec fs a = FunctorRec { getFunctorRec :: Rec (Flap a) fs }
  deriving (Show)
newtype FunctorCoRec fs a = FunctorCoRec { getFunctorCoRec :: CoRec (Flap a) fs }

deriving instance (Show (f r)) => Show (FunctorCoRec '[f] r)
deriving instance (Show (CoRec (Flap r) (g ': hs)), Show (f r)) => Show (FunctorCoRec (f ': g ': hs) r)

instance Functor (FunctorRec '[]) where
  fmap _ (FunctorRec RNil) = FunctorRec RNil

instance (Functor r, Functor (FunctorRec rs)) => Functor (FunctorRec (r ': rs)) where
  fmap f (FunctorRec (Flap r :& rs)) =
    FunctorRec (Flap (fmap f r) :& getFunctorRec (fmap f (FunctorRec rs)))

instance Applicative (FunctorRec '[]) where
  pure _ = FunctorRec RNil
  FunctorRec RNil <*> FunctorRec RNil = FunctorRec RNil

instance (Applicative r, Applicative (FunctorRec rs)) => Applicative (FunctorRec (r ': rs)) where
  pure a = FunctorRec (Flap (pure a) :& getFunctorRec (pure a))
  FunctorRec (Flap f :& fs) <*> FunctorRec (Flap a :& as) =
    FunctorRec (Flap (f <*> a) :& getFunctorRec (FunctorRec fs <*> FunctorRec as))

instance Contravariant (FunctorRec '[]) where
  contramap _ (FunctorRec RNil) = FunctorRec RNil

instance (Contravariant r, Contravariant (FunctorRec rs)) => Contravariant (FunctorRec (r ': rs)) where
  contramap f (FunctorRec (Flap r :& rs)) =
    FunctorRec (Flap (contramap f r) :& getFunctorRec (contramap f (FunctorRec rs)))

instance Functor r => Functor (FunctorCoRec '[r]) where
  fmap f (FunctorCoRec (CoRecHere (Flap a))) =
    FunctorCoRec (CoRecHere (Flap (fmap f a)))

instance (Functor f, Functor (FunctorCoRec (g ': fs))) => Functor (FunctorCoRec (f ': g ': fs)) where
  fmap f (FunctorCoRec cr) = FunctorCoRec $ case cr of
    CoRecHere (Flap v) -> CoRecHere (Flap (fmap f v))
    CoRecThere cr -> CoRecThere (getFunctorCoRec (fmap f (FunctorCoRec cr)))

instance Contravariant r => Contravariant (FunctorCoRec '[r]) where
  contramap f (FunctorCoRec (CoRecHere (Flap a))) =
    FunctorCoRec (CoRecHere (Flap (contramap f a)))

instance (Contravariant f, Contravariant (FunctorCoRec (g ': fs))) => Contravariant (FunctorCoRec (f ': g ': fs)) where
  contramap f (FunctorCoRec cr) = FunctorCoRec $ case cr of
    CoRecHere (Flap v) -> CoRecHere (Flap (contramap f v))
    CoRecThere cr -> CoRecThere (getFunctorCoRec (contramap f (FunctorCoRec cr)))

