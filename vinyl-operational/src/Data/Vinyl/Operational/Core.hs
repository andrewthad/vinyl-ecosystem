{-# LANGUAGE FlexibleContexts #-}
module Data.Vinyl.Operational.Core where

import           Control.Monad
import           Control.Monad.Operational
import           Control.Monad.Operational.Interpret
import           Data.Vinyl.Types
import           Data.Vinyl.Optic.Plain.Class

data ApplyInstr m instr = ApplyInstr (forall a. instr a -> m a)

coinstr :: RElem' instr rs => instr a -> ProgramT (FunctorCoRec rs) m a
coinstr instr = singleton (FunctorCoRec (clift (Flap instr)))

castProgramT :: (Monad m, RSubset' sub super) => ProgramT (FunctorCoRec sub) m a -> ProgramT (FunctorCoRec super) m a
castProgramT prog = mapProgramT (FunctorCoRec . ccast . getFunctorCoRec) prog

coliftProgramT :: (RElem' instr rs, Monad m) => ProgramT instr m a -> ProgramT (FunctorCoRec rs) m a
coliftProgramT = mapProgramT (FunctorCoRec . clift . Flap)

recApplyInstr :: Rec (ApplyInstr m) instrs -> FunctorCoRec instrs a -> m a
recApplyInstr (_ :& rnext) (FunctorCoRec (CoRecThere cnext)) =
  recApplyInstr rnext (FunctorCoRec cnext)
recApplyInstr (ApplyInstr f :& _) (FunctorCoRec (CoRecHere (Flap instr))) =
  f instr

recAroundInstr1 :: Rec (Around m) instrs -> FunctorCoRec instrs a -> m ()
recAroundInstr1 (_ :& rnext) (FunctorCoRec (CoRecThere cnext))
  = recAroundInstr1 rnext (FunctorCoRec cnext)
recAroundInstr1 (Around f :& rnext) (FunctorCoRec (CoRecHere (Flap instr)))
  = fst (f instr)

recAroundInstr2 :: Rec (Around m) instrs -> FunctorCoRec instrs a -> a -> m ()
recAroundInstr2 (_ :& rnext) (FunctorCoRec (CoRecThere cnext)) a
  = recAroundInstr2 rnext (FunctorCoRec cnext) a
recAroundInstr2 (Around f :& rnext) (FunctorCoRec (CoRecHere (Flap instr))) a
  = snd (f instr) a

cointerpretWithMonadT :: forall m instrs b. Monad m
  => Rec (ApplyInstr m) instrs -> ProgramT (FunctorCoRec instrs) m b -> m b
cointerpretWithMonadT interpreters = eval <=< viewT
  where
  eval :: forall a. ProgramViewT (FunctorCoRec instrs) m a -> m a
  eval (Return a) = return a
  eval (m :>>= k) = recApplyInstr interpreters m
                >>= cointerpretWithMonadT interpreters . k

cointerpretAroundMonadT :: forall m b instrs. Monad m
  => Rec (Around m) instrs
  -> Rec (ApplyInstr m) instrs
  -> ProgramT (FunctorCoRec instrs) m b
  -> m b
cointerpretAroundMonadT arounds interpreters = eval <=< viewT
  where
  eval :: forall a. ProgramViewT (FunctorCoRec instrs) m a -> m a
  eval (Return a) = return a
  eval (m@(FunctorCoRec c) :>>= k) = do
    recAroundInstr1 arounds m
    a <- recApplyInstr interpreters m
    recAroundInstr2 arounds m a
    cointerpretAroundMonadT arounds interpreters (k a)


