module Data.Vinyl.Plus.Internal where

import           Data.Profunctor
import           Data.Profunctor.Choice

-- | This is copied from 'Control.Lens.Prism'.
prism :: (Choice p, Applicative f) => (b -> t) -> (s -> Either t a) -> p a (f b) -> p s (f t)
prism bt seta = dimap seta (either pure (fmap bt)) . right'
{-# INLINE prism #-}

-- | This is copied from 'Control.Lens.Prism'.
prism' :: (Choice p, Applicative f) => (b -> s) -> (s -> Maybe a) -> p a (f b) -> p s (f s)
prism' bs sma = prism bs (\s -> maybe (Left s) Right (sma s))
{-# INLINE prism' #-}

