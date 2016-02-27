module Siphon.Types where

import           Data.ByteString (ByteString)

newtype Field a = Field { getField :: ByteString -> Either String a }
data Input = Content ByteString | Newline
-- data Result = ResultError String | Result

