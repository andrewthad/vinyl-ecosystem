-- |
-- This module provides accessors and modifiers for labeled
-- records. As the module name indicates, the functions
-- provided by this module are designed to work with records
-- under the following conditions:
--
--   (1) The @VisibleTypeApplication@ extension is unavailable.
--
-- If you are working with labeled records that use 'Identity'
-- inner functor for 'TaggedFunctor', you should consider
-- using @Data.Vinyl.Optic.Plain.Class.Tagged.Proxy.Identity@ instead.
--
module Data.Vinyl.Optic.Tagged.Proxy.Functor where

import           Data.Tagged.Functor           (TaggedFunctor)
import           Data.Vinyl.Core               (Rec)
import           Data.Vinyl.Optic.Tagged.Class
import           GHC.Prim                      (Proxy#, proxy#)

lens :: forall k g f rs i v proxy. (Functor g, IxElem k rs i v)
  => proxy k -> (f v -> g (f v))
  -> Rec (TaggedFunctor f) rs -> g (Rec (TaggedFunctor f) rs)
lens _ = rlensBy' (proxy# :: Proxy# k)

get :: forall k f rs i v proxy. IxElem k rs i v => proxy k -> Rec (TaggedFunctor f) rs -> f v
get _ = rgetBy' (proxy# :: Proxy# k)

set :: forall k f rs i v proxy. IxElem k rs i v => proxy k -> f v -> Rec (TaggedFunctor f) rs -> Rec (TaggedFunctor f) rs
set _ = rsetBy' (proxy# :: Proxy# k)

modify :: forall k f rs i v proxy. IxElem k rs i v => proxy k -> (f v -> f v) -> Rec (TaggedFunctor f) rs -> Rec (TaggedFunctor f) rs
modify _ = rmodifyBy' (proxy# :: Proxy# k)




