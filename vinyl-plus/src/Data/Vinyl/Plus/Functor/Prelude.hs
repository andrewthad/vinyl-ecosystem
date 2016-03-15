module Data.Vinyl.Plus.Functor.Prelude where

import           Prelude                 hiding (foldl, head, tail, unzip, zip,
                                          zip3, zip4)

import           Data.Functor.Constant
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Monoid             (Endo (..))
import           Data.Vinyl.Core
import           Data.Vinyl.TypeLevel
import           Data.Vinyl.Types

-- This needs to be imported qualified

head :: FunctorRec (f ': fs) a -> f a
head (FunctorRec (Flap r :& _)) = r

tail :: FunctorRec (f ': fs) a -> FunctorRec fs a
tail (FunctorRec (_ :& rs)) = FunctorRec rs

cons :: f a -> FunctorRec fs a -> FunctorRec (f ': fs) a
cons r (FunctorRec rs) = FunctorRec (Flap r :& rs)

uncons :: FunctorRec (f ': fs) a -> (f a, FunctorRec fs a)
uncons (FunctorRec (Flap r :& rs)) = (r, FunctorRec rs)

append :: FunctorRec fs a -> FunctorRec gs a -> FunctorRec (fs ++ gs) a
append (FunctorRec RNil) gs = gs
append (FunctorRec (f :& fsNext)) gs =
  FunctorRec (f :& getFunctorRec (append (FunctorRec fsNext) gs))

singleton :: f a -> FunctorRec '[f] a
singleton f = FunctorRec (Flap f :& RNil)

