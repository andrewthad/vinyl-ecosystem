{-# LANGUAGE CPP                     #-}
{-# LANGUAGE FlexibleInstances       #-}

{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE UndecidableInstances    #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif

module Data.Tuple.TypeLevel where

import           Data.Proxy
import           GHC.Exts   (Constraint)

-- | First element of a type pair.
type family Fst (k :: (m,n)) where
    Fst '(a,b) = a

-- |Second element of a type pair.
type family Snd (k :: (m,n)) where
    Snd '(a,b) = b

class c (Fst x) => ConstrainFst (c :: j -> Constraint) (x :: (j,k))
instance c (Fst x) => ConstrainFst c x

class c (Snd x) => ConstrainSnd (c :: k -> Constraint) (x :: (j,k))
instance c (Snd x) => ConstrainSnd c x

proxyFst :: proxy x -> Proxy (Fst x)
proxyFst _ = Proxy

proxySnd :: proxy x -> Proxy (Snd x)
proxySnd _ = Proxy

