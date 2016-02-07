module Control.Monad.Operational.Wait where

import           Control.Concurrent        (threadDelay)
import           Control.Monad.Operational

-- Int is the time in microseconds
data WaitI a where
  WaitI :: Int -> WaitI ()

wait :: Int -> ProgramT WaitI m ()
wait i = singleton (WaitI i)

interpretWaitIO :: WaitI a -> IO a
interpretWaitIO (WaitI i) = threadDelay i

interpretWaitIgnore :: Applicative m => WaitI a -> m a
interpretWaitIgnore (WaitI _) = pure ()


