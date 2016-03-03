-- |
-- This module provides accessors and modifiers for labeled
-- records. As the module name indicates, the functions
-- provided by this module are designed to work with records
-- under the following conditions:
--
--   (1) The 'TaggedFunctor' is parameterized by 'Identity'.
--   (2) The @VisibleTypeApplication@ extension is unavailable.
--

module Data.Vinyl.Optic.Tagged.Proxy.Identity
  (
  -- * Functions
    lens
  , get
  , set
  , modify
  -- * Tutorial
  -- $tutorial
  ) where

-- import           Data.Coerce
import           Data.Functor.Identity   (Identity (..))
import           Data.Tagged.Functor     (TaggedFunctor)
import           Data.Vinyl.Core         (Rec)
import           Data.Vinyl.Optic.Tagged.Class
import           GHC.Prim                (Proxy#, proxy#)

lens :: forall k g rs i v proxy. (Functor g, IxElem k rs i v)
  => proxy k -> (v -> g v)
  -> Rec (TaggedFunctor Identity) rs -> g (Rec (TaggedFunctor Identity) rs)
lens _ conv = rlensBy' (proxy# :: Proxy# k) (fmap Identity . conv . runIdentity)

get :: forall k rs i v proxy. IxElem k rs i v => proxy k -> Rec (TaggedFunctor Identity) rs -> v
get _ rec = runIdentity (rgetBy' (proxy# :: Proxy# k) rec)

set :: forall k rs i v proxy. IxElem k rs i v => proxy k -> v -> Rec (TaggedFunctor Identity) rs -> Rec (TaggedFunctor Identity) rs
set _ newVal rec = rsetBy' (proxy# :: Proxy# k) (Identity newVal) rec

modify :: forall k rs i v proxy. IxElem k rs i v => proxy k -> (v -> v) -> Rec (TaggedFunctor Identity) rs -> Rec (TaggedFunctor Identity) rs
modify _ f rec = rmodifyBy' (proxy# :: Proxy# k) (Identity . f . runIdentity) rec

{- $tutorial

Here is a explanation of how the functions in this module can
be used. First we will create a record:

>>> import Data.Tagged.Functor
>>> import Data.Functor.Identity
>>> import Data.Proxy
>>> import Data.Vinyl.Core
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

The 'Identity' wrappers are cumbersome to deal with. This module
provides extra functions (suffixed with @Id@) that get eliminate some
of the boilerplate. With these functions, the above becomes:

>>> get (Proxy :: Proxy "name") person
"Alexa"
>>> let deceased2 = set (Proxy :: Proxy "alive") False person
>>> get (Proxy :: Proxy "alive") deceased2
False
>>> let older2 = modify (Proxy :: Proxy "age") (+12) person
>>> get (Proxy :: Proxy "age") older2
56

-}


