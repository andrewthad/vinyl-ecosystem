{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Vinyl.Plus.CoRec where

import           Data.Vinyl.Core
import           Data.Vinyl.TypeLevel

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

class i ~ RIndex r rs => CElemX (r :: k) (rs :: [k]) (i :: Nat) where
  clift :: f r -> CoRec f rs
  cget  :: proxy r -> CoRec f rs -> Maybe (f r)
  cput  :: f r -> CoRec f rs -> CoRec f rs
  -- TODO: add clens. I'm not good with that kind of thing.

instance (r ~ s) => CElemX r (s ': rs) 'Z where
  clift  = CoRecHere
  cget _ (CoRecHere v)    = Just v
  cget _ (CoRecThere _)   = Nothing
  cput v (CoRecHere _)    = CoRecHere v
  cput _ r@(CoRecThere _) = r

instance (RIndex r (s ': rs) ~ 'S i, CElemX r rs i) => CElemX r (s ': rs) ('S i) where
  clift v = CoRecThere (clift v)
  cget _ (CoRecHere _)      = Nothing
  cget proxy (CoRecThere c) = cget proxy c
  cput _ r@(CoRecHere _)    = r
  cput v (CoRecThere r) = CoRecThere (cput v r)

class is ~ RImage sub super => CSubsetX (sub :: [k]) (super :: [k]) is where
  ccast :: CoRec f sub -> CoRec f super

instance CSubsetX '[] super '[] where
  ccast _ = error "CSubset: an empty CoRec is not possible"

instance (CElemX r super i , CSubsetX sub super is) => CSubsetX (r ': sub) super (i ': is) where
  ccast (CoRecHere v) = clift v
  ccast (CoRecThere cr) = ccast cr

type CElem r rs = CElemX r rs (RIndex r rs)
type CSubset sub super = CSubsetX sub super (RImage sub super)

-- example :: CoRec Maybe '[Bool,Int,Double]
-- example = clift (Just (4 :: Int))
--
-- example2 :: CoRec Maybe '[Bool,Int] -> CoRec Maybe '[Int,Bool,Char]
-- example2 = ccast

-- class ToCoRec r rs where
--   toCoRec :: f r -> CoRec f rs
--   fromCoRec :: CoRec f rs -> Maybe (f r)
-- instance {-# OVERLAPPABLE #-} ToCoRec r rs => ToCoRec r (a ': rs) where
--   toCoRec r = CoRecThere (toCoRec r)
--   fromCoRec (CoRecHere _) = Nothing
--   fromCoRec (CoRecThere cnext) = fromCoRec cnext
-- instance {-# OVERLAPPING #-} ToCoRec r (r ': rs) where
--   toCoRec = CoRecHere
--   fromCoRec (CoRecHere v) = Just v
--   fromCoRec (CoRecThere _) = Nothing
--

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


