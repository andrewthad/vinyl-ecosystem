{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Vinyl.Plus.CoRec where

import           Data.Vinyl.Core
import           Data.Vinyl.TypeLevel
import           Data.Vinyl.Plus.TypeLevel
import           Data.Vinyl.Lens

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

-- | This is equivalent to the 'RElem' class from @Data.Vinyl.Lens@.
--   The functions it provides work on 'CoRec's instead of 'Rec's.
--   It also provides a lifting function 'clift', which has no
--   equivalent operation on 'Rec's. Unfortunately, we cannot
--   currently provide @cprism@ without incurring a dependency
--   on @lens@.
--   If 'CoRec' were merged into @vinyl@, this typeclass could be 
--   eliminated, and its methods could be added to 'RElem'.
class i ~ RIndex r rs => CElemX (r :: k) (rs :: [k]) (i :: Nat) where
  -- | Get a value from a 'CoRec'. Note that, unlike 'rget',
  --   this function may not find the requested element, so
  --   the result is wrapped in 'Maybe'.
  cget  :: proxy r -> CoRec f rs -> Maybe (f r)
  -- | If the element in the 'CoRec' is of a certain type,
  --   modify it. Otherwise, leave the 'CoRec' unchanged.
  cmodify :: (f r -> f r) -> CoRec f rs -> CoRec f rs
  -- | If the element in the 'CoRec' is of a certain type,
  --   replace it. This function is provided for symmetry 
  --   with 'rput', but it is not typically useful.
  --   Usually, 'clift' is more useful.
  cput  :: f r -> CoRec f rs -> CoRec f rs
  -- | Lift an element into a 'CoRec'.
  clift :: f r -> CoRec f rs
  -- TODO: add cprism. I'm not good with that kind of thing.
  -- Also, cprism is not possible without a lens dependency

instance (r ~ s) => CElemX r (s ': rs) 'Z where
  clift  = CoRecHere
  cget _ (CoRecHere v)    = Just v
  cget _ (CoRecThere _)   = Nothing
  cput v (CoRecHere _)    = CoRecHere v
  cput _ r@(CoRecThere _) = r
  cmodify f (CoRecHere v)    = CoRecHere (f v)
  cmodify _ r@(CoRecThere _) = r

instance (RIndex r (s ': rs) ~ 'S i, CElemX r rs i) => CElemX r (s ': rs) ('S i) where
  clift v = CoRecThere (clift v)
  cget _ (CoRecHere _)      = Nothing
  cget proxy (CoRecThere c) = cget proxy c
  cput _ r@(CoRecHere _)    = r
  cput v (CoRecThere r) = CoRecThere (cput v r)
  cmodify _ r@(CoRecHere _) = r
  cmodify f (CoRecThere r)  = CoRecThere (cmodify f r)

class is ~ RImage sub super => CSubsetX (sub :: [k]) (super :: [k]) is where
  -- | Upcast a 'CoRec' to another 'CoRec' that could be
  --   inhabited by additional types.
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
--
-- example3 :: Maybe Int -> CoRec Maybe '[Bool,Int] -> CoRec Maybe '[Bool,Int]
-- example3 = cput
-- 
-- example4 :: CoRec Maybe '[Bool,Int] -> Maybe (Maybe Int)
-- example4 = cget (Nothing :: Maybe Int)

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


