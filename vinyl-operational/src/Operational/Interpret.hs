module Operational.Interpret where

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

interpretWithMonadT :: forall instr m b. Monad m
  => (forall a. instr a -> m a) -> ProgramT instr m b -> m b
interpretWithMonadT f = eval <=< viewT
  where
  eval :: forall a. ProgramViewT instr m a -> m a
  eval (Return a) = return a
  eval (m :>>= k) = f m >>= interpretWithMonadT f . k

interpretAround :: forall instr m b. Monad m
  => (Around m instr)
  -> (forall a. instr a -> m a)
  -> ProgramT instr m b
  -> m b
interpretAround around@(Around aroundFunc) f = eval <=< viewT
  where
  eval :: forall a. ProgramViewT instr m a -> m a
  eval (Return a) = return a
  eval (m :>>= k) = do
    before
    a <- f m
    after a
    interpretAround around f (k a)
    where (before,after) = aroundFunc m

-- interpretWithLogging :: forall instr m b. Monad m
--   => (forall a. instr a -> m ())
--   -> (forall a. instr a -> a -> m ())
--   -> (forall a. instr a -> m a)
--   -> ProgramT instr m b -> m b
-- interpretWithLogging before after f = eval <=< viewT
--   where
--   eval :: forall a. ProgramViewT instr m a -> m a
--   eval (Return a) = return a
--   eval (m :>>= k) = do
--     before m
--     a <- f m
--     after m a
--     interpretWithLogging before after f (k a)
--
-- interpretWithCombinedLogging :: forall instr m b. Monad m
--   => (forall a. instr a -> (m (),a -> m ()))
--   -> (forall a. instr a -> m a)
--   -> ProgramT instr m b -> m b
-- interpretWithCombinedLogging func f = interpretWithLogging
--   (fst . func) (snd . func) f
--
-- emptyBefore :: Applicative m => instr a -> m ()
-- emptyBefore _ = pure ()
--
-- emptyAfter :: Applicative m => instr a -> a -> m ()
-- emptyAfter _ _ = pure ()

-- mapInstruction :: forall instr m b. Monad m
--   => (forall c. instr c -> instr c) -> ProgramT instr m b -> ProgramT instr m b
-- mapInstruction f = eval . viewT
--   where
--   eval :: forall a. ProgramViewT instr m a -> ProgramT instr m a
--   eval (Return a) = return a

