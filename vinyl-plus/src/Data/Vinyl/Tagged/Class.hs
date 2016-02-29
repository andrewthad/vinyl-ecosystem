{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Vinyl.Tagged.Class
  ( IxElem(..)
  , rsetBy'
  ) where

import           Data.Functor.Identity     (Identity (..))
import           Data.Tagged.Functor       (TaggedFunctor (..))
import           Data.Vinyl.Core
import           Data.Vinyl.Plus.TypeLevel
import           Data.Vinyl.TypeLevel
import           GHC.Prim                  (Proxy#, proxy#)

-- | This is a typeclass in the spirit of 'RElem' that provides
--   a lens for 'Rec' whose values a tagged by an additional
--   marker (often a 'Symbol'). The methods in this typeclass
--   are not intended to be used directly. Instead, import one
--   of the four submodules, and use the functions it provides.
class (i ~ TIndex a rs, b ~ Lookup a rs) => IxElem (a :: k) (rs :: [(k,v)]) (i :: Nat) (b :: v) where
  rlensBy'   :: Functor g
             => Proxy# a
             -> (f b -> g (f b))
             -> Rec (TaggedFunctor f) rs
             -> g (Rec (TaggedFunctor f) rs)
  rgetBy'    :: Proxy# a -> Rec (TaggedFunctor f) rs -> f b
  rmodifyBy' :: Proxy# a -> (f b -> f b) -> Rec (TaggedFunctor f) rs -> Rec (TaggedFunctor f) rs

instance (r ~ '(k,v)) => IxElem k (r ': rs) 'Z v where
  rlensBy' _ f (TaggedFunctor x :& xs) = fmap ((:& xs) . TaggedFunctor) (f x)
  rgetBy' _ (TaggedFunctor r :& _) = r
  rmodifyBy' _ f (TaggedFunctor r :& rs) = TaggedFunctor (f r) :& rs

instance (TIndex k (s ': rs) ~ 'S i, Lookup k (s ': rs) ~ v, IxElem k rs i v)
    => IxElem k (s ': rs) ('S i) v where
  rlensBy' p f (x :& xs) = fmap (x :&) (rlensBy' p f xs)
  rgetBy' p (_ :& rs) = rgetBy' p rs
  rmodifyBy' p f (r :& rs) = r :& rmodifyBy' p f rs

rsetBy' :: IxElem k rs i v => Proxy# k -> f v -> Rec (TaggedFunctor f) rs -> Rec (TaggedFunctor f) rs
rsetBy' p newVal rec = rmodifyBy' p (const newVal) rec

