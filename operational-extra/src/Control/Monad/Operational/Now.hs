module Control.Monad.Operational.Now where

import           Control.Monad.Operational
import           Data.IORef                (IORef, newIORef, readIORef,
                                            writeIORef)
import           Data.Time                 (NominalDiffTime, UTCTime,
                                            addUTCTime, getCurrentTime)

data NowI a where
  NowI :: NowI UTCTime

interpretNow :: NowI a -> IO a
interpretNow NowI = getCurrentTime

interpretNowMoving :: UTCTime -> NominalDiffTime -> IO (NowI a -> IO a)
interpretNowMoving start interval = do
  timeRef <- newIORef start
  return (interpretNowMoving' timeRef interval)

interpretNowMoving' :: IORef UTCTime -> NominalDiffTime -> NowI a -> IO a
interpretNowMoving' timeRef interval NowI = do
  time <- readIORef timeRef
  writeIORef timeRef (addUTCTime interval time)
  return time

interpretNowConstant :: Applicative m => UTCTime -> NowI a -> m a
interpretNowConstant time NowI = pure time

