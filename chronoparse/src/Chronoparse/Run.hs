{-# LANGUAGE FlexibleContexts #-}

module Chronoparse.Run where

import           Chronoparse.Types
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as Parser
import           Data.ByteString            (ByteString)
import           Data.Vinyl.Core

parse ::
  ( StripNonesClass rs
  , CanonizeChronosClass (StripNones rs)
  , ChronoConvertable (CanonizeChronos (StripNones rs))
  ) => Rec Chronoparser rs -> ByteString -> Either String (ChronoTimeRep (CanonizeChronos (StripNones rs)))
parse rec bs = fmap (fromChronoRec . canonizeChronos . stripNones) (recParse rec bs)

recParse :: Rec Chronoparser rs -> ByteString -> Either String (Rec Chrono rs)
recParse rec bs = (flip Parser.parseOnly bs . rtraverse getChronoparser) rec





