module Operational.Http.Log where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty (Config (..), encodePretty')
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as ByteString
import qualified Data.ByteString.Lazy     as LByteString
import           Data.CaseInsensitive     (CI)
import qualified Data.CaseInsensitive     as CI
import           Data.Foldable
import           Data.IORef
import qualified Data.Map                 as Map
import           Data.Text.Encoding       (decodeUtf8)
import qualified Data.Yaml                as Yaml
import           Network.HTTP.Client
import           Network.HTTP.Types
import           Operational.Http
import           Operational.Interpret

httpLogging :: String -> IORef Int -> Around IO HttpI
httpLogging dir counter = Around $ \(HttpI request) -> let
  before = do
    i <- readIORef counter
    ByteString.writeFile
      (mkBaseDir i ++ ".request.yaml")
      (Yaml.encode (requestToJson request))
    let bodyPath = mkBaseDir i ++ ".request.body"
    case requestBody request of
      RequestBodyBS bs -> ByteString.writeFile bodyPath bs
      RequestBodyLBS lbs -> LByteString.writeFile bodyPath lbs
      _ -> return () -- Don't write the body.
  after response = do
    i <- readIORef counter
    ByteString.writeFile
      (mkBaseDir i ++ ".response.yaml")
      (Yaml.encode (responseToJson response))
    let body = responseBody response
    LByteString.writeFile (mkBaseDir i ++ ".response.body.raw") body
    for_ (guessResponseType body) $ \(formattedBody,ext) ->
      LByteString.writeFile (mkBaseDir i ++ ".response.body." ++ ext) formattedBody
    writeIORef counter (i + 1)
  mkBaseDir i = dir ++ "/" ++ lpad 6 '0' (show i)
  in (before,after)

-- The string is the extension to use
guessResponseType :: LByteString.ByteString -> Maybe (LByteString.ByteString,String)
guessResponseType bs = case decode bs of
  Just (v :: Value) -> Just (encodePretty' (Config 2 compare) v, "json")
  Nothing -> Nothing -- should also add xml later

requestToJson :: Request -> Value
requestToJson r = object
  [ "method"  .= byteStringToJson (method r)
  , "secure"  .= secure r
  , "headers" .= headersToJson (requestHeaders r)
  , "path"    .= byteStringToJson (path r)
  , "host"    .= byteStringToJson (host r)
  , "port"    .= port r
  ]

lpad :: Int -> a -> [a] -> [a]
lpad m v xs = reverse $ rpad m v $ reverse xs

rpad :: Int -> a -> [a] -> [a]
rpad m v xs = take m $ (take m xs) ++ repeat v

byteStringToJson :: ByteString -> Value
byteStringToJson b = toJSON (decodeUtf8 b)

responseToJson :: Response body -> Value
responseToJson r = object
  [ "headers" .= headersToJson (responseHeaders r)
  , "status"  .= statusCodeToJson (responseStatus r)
  , "version" .= httpVersionToJson (responseVersion r)
  ]

httpVersionToJson :: HttpVersion -> Value
httpVersionToJson v = object
  [ "major" .= httpMajor v
  , "minor" .= httpMinor v
  ]

statusCodeToJson :: Status -> Value
statusCodeToJson s = object
  [ "code"    .= statusCode s
  , "message" .= byteStringToJson (statusMessage s)
  ]

headersToJson :: [(CI ByteString, ByteString)] -> Value
headersToJson = id
  . toJSON
  . Map.fromList
  . map (\(k,v) -> (decodeUtf8 $ CI.original k, decodeUtf8 v))
