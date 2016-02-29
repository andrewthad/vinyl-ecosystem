{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vinyl.Plus.Tagged 
  ( -- * Types and Functions
    IxElem(..)
  -- * Explanation
  -- $explanation
  ) where

import           Data.Vinyl.Core
import           Data.Vinyl.TypeLevel
import           Data.Vinyl.Plus.TypeLevel
import           Data.Tagged.Functor (TaggedFunctor(..))

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

{- $explanation

Here is a explanation of how the functions in this module can
be used. First we will create a record:

>>> import Data.Tagged.Functor
>>> import Data.Proxy
>>> import Data.Functor.Identity 
>>> :{
let person = tagIdentity (Proxy :: Proxy "age") (44 :: Int)
          :& tagIdentity (Proxy :: Proxy "name") ("Alexa" :: String)
          :& tagIdentity (Proxy :: Proxy "alive") True
          :& RNil
:}

Notice that the type of @person@ is inferred:

>>> :t person
person 
  :: Rec 
       (TaggedFunctor Identity) 
       '['("age", Int), '("name", String), '("alive", Bool)]

  That\'s all.
-}


