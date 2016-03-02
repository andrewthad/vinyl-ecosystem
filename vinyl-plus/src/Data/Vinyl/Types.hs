{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Vinyl.Types where

import           Data.Functor.Classes
import           Data.Vinyl.Core      (Rec)
import           Prelude

newtype Flap a f = Flap { getFlap :: f a }
  deriving Show

-- | 'CoRec' is a generalized coproduct. The value it holds
--   is the interpretation function @f@ applied to exactly one
--   of the types in the list. While a 'Rec' can be thought of
--   a nesting of tuples, a 'CoRec' can be thought of as a
--   nesting of 'Either's.
data CoRec :: (u -> *) -> [u] -> * where
  CoRecHere  :: !(f r) -> CoRec f (r ': rs)
  CoRecThere :: !(CoRec f rs) -> CoRec f (r ': rs)

-- For monoid, the item on top is mempty. Doing the
-- bottom one would require RecApplicative.
instance (Monoid (f r)) => Monoid (CoRec f '[r]) where
  mempty = CoRecHere mempty
  mappend (CoRecHere a) (CoRecHere b) = CoRecHere (mappend a b)

instance (Monoid (CoRec f (s ' : rs)), Monoid (f r)) => Monoid (CoRec f (r ': s ': rs)) where
  mempty = CoRecHere mempty
  mappend a b = case a of
    CoRecHere aVal -> case b of
      CoRecHere bVal -> CoRecHere (mappend aVal bVal)
      CoRecThere bCr -> CoRecThere bCr
    CoRecThere aCr -> case b of
      CoRecHere _ -> CoRecThere aCr
      CoRecThere bCr -> CoRecThere (mappend aCr bCr)

instance (Show (f r)) => Show (CoRec f '[r]) where
  show (CoRecHere a) = "CoRecHere (" ++ show a ++ ")"
instance (Show (CoRec f (s ': rs)), Show (f r)) => Show (CoRec f (r ': s ': rs)) where
  show (CoRecHere a) = "CoRecHere (" ++ show a ++ ")"
  show (CoRecThere cr) = "CoRecThere (" ++ show cr ++ ")"

