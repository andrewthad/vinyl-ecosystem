{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- |
-- This module provides convenience functions for accessing and
-- modifying elements of a record whose values are addressed by
-- some key.
--
-- There is a naming convention

module Data.Vinyl.Tagged
  ( -- * Types and Functions
    IxElem(..)
  -- * Explanation
  -- $explanation
  ) where

import           Data.Functor.Identity     (Identity (..))
import           Data.Tagged.Functor       (TaggedFunctor (..))
import           Data.Vinyl.Core
import           Data.Vinyl.Plus.TypeLevel
import           Data.Vinyl.TypeLevel

-- | This is a typeclass in the spirit of 'RElem' that provides
--   a lens for 'Rec' whose values a tagged by an additional
--   marker (often a 'Symbol').
class (i ~ TIndex a rs, b ~ Lookup a rs) => IxElem (a :: k) (rs :: [(k,v)]) (i :: Nat) (b :: v) where
  rlensBy'   :: Functor g
             => proxy a
             -> (f b -> g (f b))
             -> Rec (TaggedFunctor f) rs
             -> g (Rec (TaggedFunctor f) rs)
  rgetBy'    :: proxy a -> Rec (TaggedFunctor f) rs -> f b
  rmodifyBy' :: proxy a -> (f b -> f b) -> Rec (TaggedFunctor f) rs -> Rec (TaggedFunctor f) rs

instance (r ~ '(k,v)) => IxElem k (r ': rs) 'Z v where
  rlensBy' _ f (TaggedFunctor x :& xs) = fmap ((:& xs) . TaggedFunctor) (f x)
  rgetBy' _ (TaggedFunctor r :& _) = r
  rmodifyBy' _ f (TaggedFunctor r :& rs) = TaggedFunctor (f r) :& rs

instance (TIndex k (s ': rs) ~ 'S i, Lookup k (s ': rs) ~ v, IxElem k rs i v)
    => IxElem k (s ': rs) ('S i) v where
  rlensBy' p f (x :& xs) = fmap (x :&) (rlensBy' p f xs)
  rgetBy' p (_ :& rs) = rgetBy' p rs
  rmodifyBy' p f (r :& rs) = r :& rmodifyBy' p f rs

rsetBy' :: IxElem k rs i v => proxy k -> f v -> Rec (TaggedFunctor f) rs -> Rec (TaggedFunctor f) rs
rsetBy' p newVal rec = rmodifyBy' p (const newVal) rec

rmodifyById' :: IxElem k rs i v => proxy k -> (v -> v) -> Rec (TaggedFunctor Identity) rs -> Rec (TaggedFunctor Identity) rs
rmodifyById' p f rec = rmodifyBy' p (Identity . f . runIdentity) rec

rsetById' :: IxElem k rs i v => proxy k -> v -> Rec (TaggedFunctor Identity) rs -> Rec (TaggedFunctor Identity) rs
rsetById' p newVal rec = rsetBy' p (Identity newVal) rec

rgetById' :: IxElem k rs i v => proxy k -> Rec (TaggedFunctor Identity) rs -> v
rgetById' p rec = runIdentity (rgetBy' p rec)

{- $explanation

Here is a explanation of how the functions in this module can
be used. First we will create a record:

>>> import Data.Tagged.Functor
>>> import Data.Functor.Identity
>>> import Data.Proxy
>>> :{
let person = tagIdentity (Proxy :: Proxy "age") (44 :: Int)
          :& tagIdentity (Proxy :: Proxy "name") ("Alexa" :: String)
          :& tagIdentity (Proxy :: Proxy "alive") True
          :& RNil
:}

Notice that the type of @person@ is inferred and fully monomorphic:

>>> :t person
person
  :: Rec
       (TaggedFunctor Identity)
       '['("age", Int), '("name", String), '("alive", Bool)]

Even without using the @lens@ library, we can inspect and modify
the fields in @person@:

>>> runIdentity (rgetBy' (Proxy :: Proxy "name") person)
"Alexa"
>>> let deceased1 = rsetBy' (Proxy :: Proxy "alive") (Identity False) person
>>> runIdentity (rgetBy' (Proxy :: Proxy "alive") deceased1)
False
>>> let older1 = rmodifyBy' (Proxy :: Proxy "age") (\(Identity a) -> Identity (a + 12)) person
>>> runIdentity (rgetBy' (Proxy :: Proxy "age") older1)
56

The 'Identity' wrappers are cumbersome to deal with. This module
provides extra functions (suffixed with @Id@) that get eliminate some
of the boilerplate. With these functions, the above becomes:

>>> rgetById' (Proxy :: Proxy "name") person
"Alexa"
>>> let deceased2 = rsetById' (Proxy :: Proxy "alive") False person
>>> rgetById' (Proxy :: Proxy "alive") deceased2
False
>>> let older2 = rmodifyById' (Proxy :: Proxy "age") (+12) person
>>> rgetById' (Proxy :: Proxy "age") older2
56

-}


