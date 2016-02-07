module Operational.Http where

import           Control.Exception            (try)
import           Control.Monad.Operational
import           Data.Aeson                   (withText)
import           Data.Aeson.Types             (Parser)
import qualified Data.ByteString              as ByteString
import qualified Data.ByteString.Lazy         as LByteString
import qualified Data.Map                     as Map
import qualified Data.Text.Encoding           as Text
import           Network.HTTP.Client
import           Network.HTTP.Client.Internal (Response (..),
                                               ResponseClose (..))
import           Network.HTTP.Types.Status    (Status (..))
import           Network.HTTP.Types.Version   (HttpVersion (..))
import           Operational.Interpret

-- Consider creating an HttpJsonI at some point.

data HttpI a where
  HttpI :: Request -> HttpI (Response LByteString.ByteString)

http :: Request -> ProgramT HttpI m (Response LByteString.ByteString)
http req = singleton (HttpI req)

interpretHttp :: Manager -> HttpI a -> IO a
interpretHttp mngr (HttpI req) = httpLbs req mngr

interpretHttpWith :: Manager -> (Request -> Request) -> HttpI a -> IO a
interpretHttpWith mngr f (HttpI req) = httpLbs (f req) mngr

interpretHttpCatch :: (HttpException -> Manager -> HttpI a -> IO a)
                   -> Manager -> HttpI a -> IO a
interpretHttpCatch f mngr i@(HttpI req) = do
  e <- try (httpLbs req mngr)
  case e of
    Left httpException -> f httpException mngr i
    Right a -> return a

runHttp :: Manager -> ProgramT HttpI IO a -> IO a
runHttp mngr = interpretWithMonadT (interpretHttp mngr)

