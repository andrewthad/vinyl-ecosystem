module Operational.CoRec where

import           Control.Monad
import           Control.Monad.Operational
import           Data.CoRec
import           Data.Vinyl.Core
import           Operational.Interpret

newtype Flip a f = Flip (f a)
newtype CoInstr rs a = CoInstr { getCoInstr :: CoRec (Flip a) rs }
data ApplyInstr m instr = ApplyInstr (forall a. instr a -> m a)

coInstr :: ToCoRec instr rs => instr a -> ProgramT (CoInstr rs) m a
coInstr instr = singleton (CoInstr (toCoRec (Flip instr)))


-- TODO: figure out a more general way to write these.
applyCoInstr :: Rec (ApplyInstr m) instrs -> CoInstr instrs a -> m a
applyCoInstr (_ :& rnext) (CoInstr (CoRecThere cnext)) = applyCoInstr rnext (CoInstr cnext)
applyCoInstr (ApplyInstr f :& _) (CoInstr (CoRecHere (Flip instr))) = f instr

applyCoInstrAround1 :: Rec (Around m) instrs -> CoInstr instrs a -> m ()
applyCoInstrAround1 (_ :& rnext) (CoInstr (CoRecThere cnext))
  = applyCoInstrAround1 rnext (CoInstr cnext)
applyCoInstrAround1 (Around f :& rnext) (CoInstr (CoRecHere (Flip instr)))
  = fst (f instr)

applyCoInstrAround2 :: Rec (Around m) instrs -> CoInstr instrs a -> a -> m ()
applyCoInstrAround2 (_ :& rnext) (CoInstr (CoRecThere cnext)) a
  = applyCoInstrAround2 rnext (CoInstr cnext) a
applyCoInstrAround2 (Around f :& rnext) (CoInstr (CoRecHere (Flip instr))) a
  = snd (f instr) a

coInterpretWithMonad :: forall m instrs b. Monad m
  => Rec (ApplyInstr m) instrs -> Program (CoInstr instrs) b -> m b
coInterpretWithMonad interpreters = eval . view
  where
  eval :: forall a. ProgramView (CoInstr instrs) a -> m a
  eval (Return a) = return a
  eval (m :>>= k) = applyCoInstr interpreters m
                >>= coInterpretWithMonad interpreters . k

coInterpretWithMonadT :: forall m instrs b. Monad m
  => Rec (ApplyInstr m) instrs -> ProgramT (CoInstr instrs) m b -> m b
coInterpretWithMonadT interpreters = eval <=< viewT
  where
  eval :: forall a. ProgramViewT (CoInstr instrs) m a -> m a
  eval (Return a) = return a
  eval (m :>>= k) = applyCoInstr interpreters m
                >>= coInterpretWithMonadT interpreters . k

coInterpretAround :: forall m b instrs. Monad m
  => Rec (Around m) instrs
  -> Rec (ApplyInstr m) instrs
  -> ProgramT (CoInstr instrs) m b
  -> m b
coInterpretAround arounds interpreters = eval <=< viewT
  where
  eval :: forall a. ProgramViewT (CoInstr instrs) m a -> m a
  eval (Return a) = return a
  eval (m@(CoInstr c) :>>= k) = do
    applyCoInstrAround1 arounds m
    a <- applyCoInstr interpreters m
    applyCoInstrAround2 arounds m a
    coInterpretAround arounds interpreters (k a)

coInterpretAroundAlt :: Monad m
  => (Around m (CoInstr instrs))
  -> Rec (ApplyInstr m) instrs
  -> ProgramT (CoInstr instrs) m b
  -> m b
coInterpretAroundAlt = error "coInterpretAroundAlt: write this"

coLogging :: forall m rs instr. (Monad m, ToCoRec instr rs)
  => (forall a. instr a      -> (m (), a -> m ()))
  -> (forall a. CoInstr rs a -> (m (), a -> m ()))
coLogging logger (CoInstr c) = case fromCoRec c of
  Nothing -> (return (), \_ -> return ())
  Just (Flip n) -> logger n

-- interpretWithMonad :: forall instr m b.
--     Monad m => (forall a. instr a -> m a) -> (Program instr b -> m b)
-- interpretWithMonad f = eval . view
--     where
--     eval :: forall a. ProgramView instr a -> m a
--     eval (Return a) = return a
--     eval (m :>>= k) = f m >>= interpretWithMonad f . k


