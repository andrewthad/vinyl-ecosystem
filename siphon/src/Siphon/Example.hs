{-# LANGUAGE OverloadedStrings #-}

module Siphon.Example where

import Data.Vinyl.Core (Rec(..))
import Control.Monad.Trans.Except (runExceptT)
import qualified Siphon.Field as Siphon
import qualified Pipes.Prelude as Pipes
import Pipes
import Data.Functor.Identity
import Siphon.Types
import Siphon.Parse
import Data.Text (Text)
import Data.Tagged.Functor

p1 :: Rec Field '[Text,Int]
p1 = Siphon.text :& Siphon.int :& RNil

t1 :: Either String [Rec Identity '[Text,Int]]
t1 = runIdentity $ runExceptT $ Pipes.toListM $ xs >-> rows p1
  where
  xs = produceRows
    [ ["Drew", "24"]
    , ["Luke", "22"]
    , ["Jake", "20"]
    ]

type Person = '[ '("name", Text), '("age", Int) ]

p2 :: Rec (TaggedFunctor Field) Person
p2 = TaggedFunctor Siphon.text :& TaggedFunctor Siphon.int :& RNil

t2 :: Either String [Rec (TaggedFunctor Identity) Person]
t2 = runIdentity $ runExceptT $ Pipes.toListM $ xs >-> headedRows p2
  where
  xs = produceRows
    [ ["name", "age"]
    , ["Drew", "24"]
    , ["Luke", "22"]
    , ["Jake", "20"]
    ]

