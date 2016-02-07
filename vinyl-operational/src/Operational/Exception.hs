module Operational.Exception where

import           Control.Exception
import           Control.Monad.Operational
import           Data.Typeable

data ExceptionI a where
  ExceptionI :: SomeException -> ExceptionI a

exception :: SomeException -> ProgramT ExceptionI m a
exception e = singleton (ExceptionI e)

exception' :: Exception e => e -> ProgramT ExceptionI m a
exception' e = singleton (ExceptionI (toException e))

toExceptionI :: Exception e => e -> ExceptionI a
toExceptionI = ExceptionI . toException

strExceptionI :: String -> ExceptionI a
strExceptionI = toExceptionI . DescribedException

interpretExceptionIO :: ExceptionI a -> IO a
interpretExceptionIO (ExceptionI e) = throwIO e

data DescribedException = DescribedException String
  deriving (Show,Typeable)

instance Exception DescribedException


