{-# LANGUAGE PolyKinds #-}

module Siphon.Functor where

import Data.Tuple.TypeLevel
import Data.Vinyl

-- This module provides polykinded versions of some of 
-- the types from transformers. The goal is to remove 
-- this module at some point.

newtype Constant (a :: *) (b :: k) = Constant { getConstant :: a }
newtype Compose (f :: l -> *) (g :: k -> l) (x :: k) = Compose { getCompose :: f (g x) }
newtype PairSnd (a :: (k,*)) = PairSnd { getPairSnd :: Snd a }

data Product f g a = Pair (f a) (g a)

zipX :: Rec f rs -> Rec g rs -> Rec (Product f g) rs
zipX RNil RNil = RNil
zipX (a :& as) (b :& bs) = Pair a b :& zipX as bs



