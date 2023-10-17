{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- | Missing instances for WriterT in Agda's internals.
--
-- NB: we are not exporting any definitions, just typeclass instances.
module AgdaInternals () where

import Data.String

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State

import Agda.TypeChecking.Monad
import Agda.TypeChecking.Warnings
import Agda.TypeChecking.Pretty

instance (Monoid r, Monad m, Semigroup (m Doc)) => Semigroup (WriterT r m Doc) where
  (<>) = liftM2 (<>)
instance (Monoid r, Monad m, IsString (m a)) => IsString (WriterT r m a) where
  fromString s = WriterT $ fmap (,mempty) $ fromString s
instance (Monoid r, MonadStConcreteNames m) => MonadStConcreteNames (WriterT r m) where
  runStConcreteNames m = WriterT $ runStConcreteNames $ StateT $ \s -> do
    ((x,s'),ns) <- runWriterT $ runStateT m s
    return ((x,ns),s')
instance (Monoid r, MonadInteractionPoints m) => MonadInteractionPoints (WriterT r m)
instance (Monoid r, MonadFresh i m) => MonadFresh i (WriterT r m)
instance (Monoid r, MonadWarning m) => MonadWarning (WriterT r m) where
instance (Monoid r, MonadBlock m) => MonadBlock (WriterT r m) where
  catchPatternErr h m = WriterT $ catchPatternErr (runWriterT . h) (runWriterT m)
instance (Monoid r, MonadTCM m, MonadConstraint m) => MonadConstraint (WriterT r m) where
  addConstraint x = lift . addConstraint x
  addAwakeConstraint x = lift . addAwakeConstraint x
  solveConstraint = lift . solveConstraint
  solveSomeAwakeConstraints x = lift . solveSomeAwakeConstraints x
  wakeConstraints = lift . wakeConstraints
  stealConstraints = lift . stealConstraints
  modifyAwakeConstraints = lift . modifyAwakeConstraints
  modifySleepingConstraints = lift . modifySleepingConstraints
instance (Monoid r, MonadTCM m, MonadMetaSolver m) => MonadMetaSolver (WriterT r m) where
  newMeta' x y z k l = lift . newMeta' x y z k l
  assignV x y z w = lift . assignV x y z w
  assignTerm' x y = lift . assignTerm' x y
  etaExpandMeta x = lift . etaExpandMeta x
  updateMetaVar x = lift . updateMetaVar x
  speculateMetas x = WriterT . runWriterT . speculateMetas x
