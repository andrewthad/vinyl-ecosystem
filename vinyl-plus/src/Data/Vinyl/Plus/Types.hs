module Data.Vinyl.Plus.Types where

import           Data.Functor.Classes
import           Data.Vinyl.Core      (Rec)
import           Prelude

newtype Flap a f = Flap { getFlap :: f a }
  deriving Show

-- instance (Show1 f, Show a) => Show (Flap a f) where
--   showsPrec i (Flap v) = showString "Flap " . showsPrec1 11 v

