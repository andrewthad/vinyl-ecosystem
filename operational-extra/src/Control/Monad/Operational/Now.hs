module Control.Monad.Operational.Now where

import           Control.Monad.Operational
import           Control.Monad.Operational.Interpret
import           Data.IORef                (IORef, newIORef, readIORef,
                                            writeIORef)
import           Data.Time                 (NominalDiffTime, UTCTime,
                                            addUTCTime, getCurrentTime)

data NowI a where
  NowI :: NowI UTCTime

interpretNow :: NowI a -> IO a
interpretNow NowI = getCurrentTime

interpretNowMoving :: UTCTime -> NominalDiffTime -> IO (ApplyInstr IO NowI)
interpretNowMoving start interval = do
  timeRef <- newIORef start
  return (interpretNowMoving' timeRef interval)

interpretNowMoving' :: IORef UTCTime -> NominalDiffTime -> ApplyInstr IO NowI
interpretNowMoving' timeRef interval = ApplyInstr $ \NowI -> do
  time <- readIORef timeRef
  writeIORef timeRef (addUTCTime interval time)
  return time

interpretNowConstant :: Applicative m => UTCTime -> NowI a -> m a
interpretNowConstant time NowI = pure time

