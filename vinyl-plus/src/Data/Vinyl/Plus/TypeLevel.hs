module Data.Vinyl.Plus.TypeLevel where

import           GHC.Exts (Constraint)

type family Head (rs :: [k]) :: k where
  Head (r ': rs) = r

type family ListAll (rs :: [k]) (c :: k -> Constraint) :: Constraint where
  ListAll '[] c = ()
  ListAll (a ': as) c = (c a, ListAll as c)

