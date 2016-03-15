module Control.Monad.Operational.Console.Text where

import           Control.Monad.Operational (ProgramT)
import           Control.Monad.Operational.Console (ConsoleI(..))
import           Data.Text (Text)
import qualified Control.Monad.Operational.Console as Console

get :: ProgramT (ConsoleI Text) m Text
get = Console.get

put :: Text -> ProgramT (ConsoleI Text) m ()
put = Console.put

getRetry :: Monad m => (Text -> Text) -> (Text -> Maybe a) -> ProgramT (ConsoleI Text) m a
getRetry = Console.getRetry

