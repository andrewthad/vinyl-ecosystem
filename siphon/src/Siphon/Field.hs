module Siphon.Field where

import           Control.Monad
import qualified Data.Attoparsec.ByteString as Parser
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as ByteString
import qualified Data.ByteString.Char8      as BC8
import           Data.Text                  (Text)
import           Data.Text.Encoding         (decodeUtf8')
import           Siphon.Internal
import           Siphon.Types               (Field (..))

make :: (ByteString -> Either String a) -> Field a
make = Field

unit :: Field ()
unit = make (\_ -> Right ())

int :: Field Int
int = make (noMore "invalid int" <=< maybeToEither "invalid int" . BC8.readInt)

integer :: Field Integer
integer = make (noMore "invalid integer" <=< maybeToEither "invalid integer" . BC8.readInteger)

text :: Field Text
text = Field $ mapLeft show . decodeUtf8'

noMore :: String -> (a, ByteString) -> Either String a
noMore err (a,bs) = if ByteString.null bs
  then Right a
  else Left err

