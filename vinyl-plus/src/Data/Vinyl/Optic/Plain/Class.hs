{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Vinyl.Optic.Plain.Class where

import           Data.Profunctor.Choice    (Choice)
import           Data.Vinyl.Core
import           Data.Vinyl.Plus.Internal  (prism')
import           Data.Vinyl.Plus.TypeLevel
import           Data.Vinyl.TypeLevel
import           Data.Vinyl.Types

-- | This is a drop-in replacement for the 'RElem' class from @Data.Vinyl.Lens@.
--   The functions it provides work on 'CoRec's instead of 'Rec's.
--   It also provides a lifting function 'clift', which has no
--   equivalent operation on 'Rec's.
--   If 'CoRec' were merged into @vinyl@, this typeclass could be
--   eliminated, and its methods could be added to 'RElem'.
class i ~ RIndex r rs => RElem (r :: k) (rs :: [k]) (i :: Nat) where
  -- | We can get a prism for getting and setting values in a 'CoRec'. Morally,
  --
  -- > cprism :: Prism' (CoRec f rs) (f r)
  cprism :: (Choice p, Applicative g) => proxy r -> p (f r) (g (f r)) -> p (CoRec f rs) (g (CoRec f rs))
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

instance (r ~ s) => RElem r (s ': rs) 'Z where
  cprism p = prism' clift (cget p)
  clift  = CoRecHere
  cget _ (CoRecHere v)    = Just v
  cget _ (CoRecThere _)   = Nothing
  cput v (CoRecHere _)    = CoRecHere v
  cput _ r@(CoRecThere _) = r
  cmodify f (CoRecHere v)    = CoRecHere (f v)
  cmodify _ r@(CoRecThere _) = r

instance (RIndex r (s ': rs) ~ 'S i, RElem r rs i) => RElem r (s ': rs) ('S i) where
  cprism p = prism' clift (cget p)
  clift v = CoRecThere (clift v)
  cget _ (CoRecHere _)      = Nothing
  cget proxy (CoRecThere c) = cget proxy c
  cput _ r@(CoRecHere _)    = r
  cput v (CoRecThere r) = CoRecThere (cput v r)
  cmodify _ r@(CoRecHere _) = r
  cmodify f (CoRecThere r)  = CoRecThere (cmodify f r)

class is ~ RImage sub super => RSubset (sub :: [k]) (super :: [k]) is where
  -- | Upcast a 'CoRec' to another 'CoRec' that could be
  --   inhabited by additional types.
  ccast :: CoRec f sub -> CoRec f super

instance RSubset '[] super '[] where
  ccast _ = error "CSubset: an empty CoRec is not possible"

instance (RElem r super i , RSubset sub super is) => RSubset (r ': sub) super (i ': is) where
  ccast (CoRecHere v) = clift v
  ccast (CoRecThere cr) = ccast cr

-- type CElem r rs = CElemX r rs (RIndex r rs)
-- type CSubset sub super = CSubsetX sub super (RImage sub super)

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

