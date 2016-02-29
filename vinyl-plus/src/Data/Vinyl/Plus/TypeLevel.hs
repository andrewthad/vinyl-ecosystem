-- {-# LANGUAGE UndecidableInstances #-}
module Data.Vinyl.Plus.TypeLevel where

import           GHC.Exts (Constraint)
import           Data.Vinyl.TypeLevel

type family Head (rs :: [k]) :: k where
  Head (r ': rs) = r

type family ListAll (rs :: [k]) (c :: k -> Constraint) :: Constraint where
  ListAll '[] c = ()
  ListAll (a ': as) c = (c a, ListAll as c)

-- | A partial relation that gives the index of a value in a list.
type family Lookup (r :: k) (rs :: [(k,v)]) :: v where
  Lookup k ('(k,v) ': rs) = v
  Lookup k ('(j,v) ': rs) = (Lookup k rs)

-- | A partial relation that gives the index of a value in a list.
type family TIndex (r :: k) (rs :: [(k,v)]) :: Nat where
  TIndex r ( '(r,x) ': rs) = 'Z
  TIndex r ( '(s,x) ': rs) = 'S (TIndex r rs)


-- type family FstPlusOne a where
--   FstPlusOne '(n,x) = '( 'S n, x)

