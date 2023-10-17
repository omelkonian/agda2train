{-# LANGUAGE TemplateHaskell #-}
-- | This module contains everything related to the generation of the training data.
module ToTrain where

import Data.Maybe ( isJust )
import Data.List ( isPrefixOf, isInfixOf, find, nub )
import qualified Data.Set as S
import Data.FileEmbed ( embedStringFile )

import Control.Monad ( forM_, void, when, unless )
import Control.Monad.Writer ( WriterT(runWriterT) )
import Control.Monad.Error.Class ( catchError )
import Control.Monad.IO.Class ( liftIO )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( race )

import Agda.Syntax.Common
  ( unArg, defaultArg, defaultArgInfo, namedArg )
import Agda.Syntax.Internal
import Agda.Syntax.Internal.Names ( namesIn )
import Agda.Syntax.Literal ( Literal(..) )
import Agda.Syntax.Internal.Generic ( TermLike, foldTerm )
import Agda.Syntax.Scope.Base ( nsInScope, allThingsInScope )
import Agda.Syntax.Scope.Monad ( getCurrentScope )

import Agda.Utils.Monad ( whenM, tell1 )
import Agda.Utils.Either ( caseEitherM )

import Agda.TypeChecking.Monad hiding (Reduced)
import Agda.TypeChecking.Reduce
  ( Simplify, simplify, Normalise, normalise, Reduce, reduce )
import Agda.TypeChecking.CheckInternal ( Action(..), defaultAction, checkInternal' )
import Agda.TypeChecking.Pretty ( PrettyTCM )


import AgdaInternals ()
import Output hiding ( Definition(..), Clause(..), Term(..), Type(..) )

-- * Wrapper around Agda's typechecking monad 'TCM'

-- | Additionally records/outputs training samples.
type C = WriterT [Sample] TCM

runC :: C () -> TCM [Sample]
runC = (snd <$>) . runWriterT

noop :: C ()
noop = return ()

silently :: C a -> C ()
silently k = void k `catchError` \ _ -> noop

-- * Training data generation

-- | A training function generates training data for each typed sub-term,
-- with access to the local context via the typechecking monad.
type TrainF = Type -> Term -> C ()

-- | An example training function that records a 'Output.Sample'
-- (i.e. context, type, and term) for a given subterm.
train :: TrainF
train ty t = do
  let ns = names t
  allNs <- nsInScope . allThingsInScope <$> liftTCM getCurrentScope
  unless (null ns) $
    when (S.fromList ns `S.isSubsetOf` allNs) $ do
      ctx <- getContextTelescope
      pctx <- liftTCM $ ppm ctx; pty <- liftTCM $ ppm ty; pt <- liftTCM $ ppm t
      rty <- mkReduced ty
      report 30 $ "rty: " <> ppm (original rty)

      rt <- mkReduced t

      ctx' <- liftTCM $ convert ctx
      rty' <- liftTCM $ traverse convert rty
      rt'  <- liftTCM $ traverse convert rt
      tell1 $ Sample
        { ctx      = prender pctx :> ctx'
        , goal     = prender pty  :> rty'
        , term     = prender pt   :> rt'
        , premises = map ppName ns
        }
      report 20 "{"
      report 20 $ " ctx: " <> ppm (pp ctx)
      report 30 $ "      *pretty: " <> pure pctx
      report 20 $ " goal: " <> ppm (pp ty)
      report 30 $ "      *pretty: " <> pure pty
      reportReduced rty
      report 20 $ " term: " <> ppm (pp t)
      report 30 $ "      *pretty: " <> pure pt
      reportReduced rt
      report 20 $ " premises: " <> ppm ns
      report 20 "}"

-- | Run the training function on each subterm of a definition.
forEachHole :: TrainF -> Definition -> C ()
forEachHole trainF def@Defn{..} = unless (ignoreDef def) $ do
  report 10 $ "------ definition: " <> ppm (pp defName) <> " -------"
  sc <- getScope
  case theDef of
    (Function{..}) ->
      forM_ funClauses $ \c@(Clause{..}) -> addContext clauseTel $
        case (clauseBody, unArg <$> clauseType) of
          (Just t, Just ty) -> go ty t
          _ -> noop
    _ -> noop
    -- TODO: get data from other places other than clause bodies
  where
    ignoreDef :: Definition -> Bool
    ignoreDef Defn{..}
        = False
       || tooSlow (pp defName)
      -- || defCopy
      -- || defNoCompilation
      -- || null (inverseScopeLookupName defName sc)
      -- || isAnonymousModuleName (qnameModule defName)
      -- || ("._." `isInfixOf` pp defName)
      -- || (getOrigin defName /= UserWritten)
      -- || ( ("with-" `isPrefixOf` pp (qnameName defName))

    ignore :: Type -> C Bool
    ignore ty = do
      ctx <- fmap (snd . unDom) <$> getContext
      return (ignoreType ty || any ignoreCtxType ctx)

    ignoreType, ignoreCtxType :: Type -> Bool
    ignoreType    = any cubicalRelated . map pp . names . unEl
    ignoreCtxType = any cubicalRelated . map pp . names . unEl

    cubicalRelated, tooSlow :: String -> Bool
    cubicalRelated = ("Agda.Primitive.Cubical.I" `isInfixOf`)
    tooSlow
       = ("Data.Rational.Properties" `isPrefixOf`)
      \/ ("Prelude.Solvers" `isPrefixOf`)
      \/ ("foundation.partitions" `isPrefixOf`)
      \/ (`elem` defsToSkip)

    go :: Type -> Term -> C ()
    go ty t = whenM (not <$> ignore ty)
            $ silently (checkInternal' act t CmpLeq ty)

    act :: Action C
    act = defaultAction {preAction = pre}

    pre :: Type -> Term -> C Term
    pre ty t = trainF ty t >> return t

-- | Read a list of definitions to skip from @data/defsToSkip.txt@.
defsToSkip :: [String]
defsToSkip = lines $ $(embedStringFile "data/defsToSkip.txt")

-- | Gathering names from terms.
names :: Term -> [QName]
names = nub . namesIn

-- * Reduction

-- | The hard limit on how much time can be spent on normalising a single term.
maxDuration = 2 -- seconds

withTimeout :: TCM a -> TCM (Maybe a)
withTimeout k = getTC >>= \ s -> liftIO $
  caseEitherM
    (race (threadDelay (maxDuration * 1000000))
          (fst <$> runSafeTCM k s))
    (\() -> pure Nothing)
    (pure . Just)

mkReduced ::
  ( MonadFail m, MonadTCM m, PrettyTCM a
  , Simplify a, Reduce a, Normalise a, Eq a
  ) => a -> m (Reduced a)
mkReduced t = do
  -- try different reductions (with timeout)
  [_, simplified, reduced, normalised] <- compressList . (Just t :)
    <$> mapM (liftTCM . withTimeout) [simplify t, reduce t, normalise t]
  return $ Reduced {original = t, ..}
  where
    compressList :: Eq a => [Maybe a] -> [Maybe a]
    compressList xs = find ((`notElem` xs) . Just) <$> xs

reportReduced :: (MonadTCM m, PrettyTCM a) => Reduced a -> m ()
reportReduced Reduced{..} = do
  report 30 $ "  *simplified: " <> ppm simplified
  report 30 $ "     *reduced: " <> ppm reduced
  report 30 $ "  *normalised: " <> ppm normalised
