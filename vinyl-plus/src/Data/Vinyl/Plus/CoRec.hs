{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Vinyl.Plus.CoRec where

import           Data.Vinyl.Core

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

-- instance (Show1 f, Show r) => Show (CoRec f '[r]) where
--   showsPrec i (CoRecHere a) = id
--     . showString "CoRecHere " . showsPrec1 11 a
--
-- instance (Show (CoRec f rs), Show1 f, Show r, Show s) => Show (CoRec f (r ': s ': rs)) where
--   showsPrec i (CoRecHere a) = showString "CoRecHere " . showsPrec1 11 a
--   showsPrec i (CoRecThere cr1) = case cr1 of
--     CoRecHere a -> showString "CoRecHere " . showsPrec1 11 a
--     CoRecThere cr2 -> showString "CoRecThere " . showsPrec 11 cr2
-- instance Show (CoRec f '[]) where
--   show _ = error "CoRec show: uninhabited"
-- instance (Show (CoRec f rs), Show (f r)) => Show (CoRec f (r ': rs)) where
--   show (CoRecHere a) = "CoRecHere (" ++ show a ++ ")"
--   show (CoRecThere cr) = "CoRecThere (" ++ show cr ++ ")"


class ToCoRec r rs where
  toCoRec :: f r -> CoRec f rs
  fromCoRec :: CoRec f rs -> Maybe (f r)
instance {-# OVERLAPPABLE #-} ToCoRec r rs => ToCoRec r (a ': rs) where
  toCoRec r = CoRecThere (toCoRec r)
  fromCoRec (CoRecHere _) = Nothing
  fromCoRec (CoRecThere cnext) = fromCoRec cnext
instance {-# OVERLAPPING #-} ToCoRec r (r ': rs) where
  toCoRec = CoRecHere
  fromCoRec (CoRecHere v) = Just v
  fromCoRec (CoRecThere _) = Nothing


