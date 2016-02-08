module Control.Monad.Operational.Interpret where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Operational

data Around m instr = Around (forall a. instr a -> (m (), a -> m ()))

instance Applicative m => Monoid (Around m instr) where
  mempty = Around (\_ -> (pure (), \_ -> pure ()))
  mappend (Around f) (Around g) = Around $ \instr -> let
    (f1,f2) = f instr
    (g1,g2) = g instr
    in (f1 *> g1, f2 *> g2)

mapProgramT :: Monad m => (forall b. instr1 b -> instr2 b) -> ProgramT instr1 (ProgramT instr2 m) a -> ProgramT instr2 m a
mapProgramT f prog = interpretWithMonadT (singleton . f) prog

interpretWithMonadT :: forall instr m b. Monad m
  => (forall a. instr a -> m a) -> ProgramT instr m b -> m b
interpretWithMonadT f = eval <=< viewT
  where
  eval :: forall a. ProgramViewT instr m a -> m a
  eval (Return a) = return a
  eval (m :>>= k) = f m >>= interpretWithMonadT f . k

interpretAroundMonadT :: forall instr m b. Monad m
  => (Around m instr)
  -> (forall a. instr a -> m a)
  -> ProgramT instr m b
  -> m b
interpretAroundMonadT around@(Around aroundFunc) f = eval <=< viewT
  where
  eval :: forall a. ProgramViewT instr m a -> m a
  eval (Return a) = return a
  eval (m :>>= k) = do
    before
    a <- f m
    after a
    interpretAroundMonadT around f (k a)
    where (before,after) = aroundFunc m

