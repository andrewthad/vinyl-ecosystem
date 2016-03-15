module Control.Monad.Operational.Console where

import           Control.Monad.Operational
import           Data.Text (Text)
import           Data.ByteString (ByteString)
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as LByteString

-- Int is the time in microseconds
data ConsoleI content a where
  GetI :: ConsoleI content content
  PutI :: content -> ConsoleI content ()

get :: ProgramT (ConsoleI content) m content
get = singleton GetI

put :: content -> ProgramT (ConsoleI content) m ()
put = singleton . PutI

getRetry :: Monad m => (content -> content) -> (content -> Maybe a) -> ProgramT (ConsoleI content) m a
getRetry = go 
  where 
  go failureMessage parse = do
    content <- get
    case parse content of
      Nothing -> do
        put (failureMessage content)
        go failureMessage parse
      Just a -> return a

interpretConsoleStringIO :: ConsoleI String a -> IO a
interpretConsoleStringIO instr = case instr of
  PutI content -> putStrLn content
  GetI -> getLine

interpretConsoleTextIO :: ConsoleI Text a -> IO a
interpretConsoleTextIO instr = case instr of
  PutI content -> Text.putStrLn content
  GetI -> Text.getLine

interpretConsoleLazyTextIO :: ConsoleI LText.Text a -> IO a
interpretConsoleLazyTextIO instr = case instr of
  PutI content -> LText.putStrLn content
  GetI -> LText.getLine

interpretConsoleByteStringIO :: ConsoleI ByteString a -> IO a
interpretConsoleByteStringIO instr = case instr of
  PutI content -> ByteString.putStrLn content
  GetI -> ByteString.getLine

interpretConsoleLazyByteStringIO :: ConsoleI LByteString.ByteString a -> IO a
interpretConsoleLazyByteStringIO instr = case instr of
  PutI content -> ByteString.putStrLn (LByteString.toStrict content)
  GetI -> fmap LByteString.fromStrict ByteString.getLine

