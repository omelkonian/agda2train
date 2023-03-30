module ToTrain
  ( TrainF
  , train
  , forEachHole
  , execC
  , reportTCM
  , ppm
  ) where

import Data.List ( isPrefixOf, isInfixOf )
import qualified Data.Set as S

import Control.Monad ( forM_, void, when, unless )
import Control.Monad.Writer ( WriterT(runWriterT) )
import Control.Monad.Error.Class ( catchError )
import Control.Monad.IO.Class ( liftIO )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( race )

import Text.PrettyPrint ( render )

import Agda.Syntax.Common ( unArg )
import Agda.Syntax.Internal
import Agda.Syntax.Internal.Generic ( TermLike, foldTerm )
import Agda.Syntax.Scope.Base ( nsInScope, allThingsInScope )
import Agda.Syntax.Scope.Monad ( getCurrentScope )

import Agda.Utils.Monad ( whenM, tell1 )
import Agda.Utils.Either ( caseEitherM )

import Agda.TypeChecking.Monad hiding (Reduced)
import Agda.TypeChecking.Reduce
  ( Simplify, simplify, Normalise, normalise, Reduce, reduce )
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.CheckInternal
  ( Action(..), defaultAction, checkInternal' )

import AgdaInternals ()
import Output

-- ** Extending the typechecking monad to also record/output training samples.
type C = WriterT [Sample] TCM

execC :: C () -> TCM [Sample]
execC = (snd <$>) . runWriterT

runC :: C a -> TCM a
runC = (fst <$>) . runWriterT

-- A training function generates training data for each typed sub-term,
-- with access to the local context via the typechecking monad.
type TrainF = Type -> Term -> C ()

-- an example training function that just prints the relevant (local) information
train :: TrainF
train ty t = do
  let ns = names t
  allNs <- nsInScope . allThingsInScope <$> liftTCM getCurrentScope
  unless (null ns) $
    -- TODO: we drop samples that use private definitions for now, but it would
    -- be more informative to peek into a module's private parts...
    when (S.fromList ns `S.isSubsetOf` allNs) $ do
      ctx <- getContextTelescope
      pctx <- liftTCM $ ppm ctx; pty <- liftTCM $ ppm ty; pt <- liftTCM $ ppm t
      rty <- mkReduced ty
      report 30 $ "rty: " <> ppm (original rty)

      rt <- mkReduced t
      tell1 $ Sample
        { ctx  = render pctx :> convert ctx
        , goal = render pty  :> fmap convert rty
        , term = render pt   :> fmap convert rt
        , namesUsed = map pp ns
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
      report 20 $ " namesUsed: " <> ppm ns
      report 20 "}"
  where
    mkReduced :: (PrettyTCM a, Simplify a, Reduce a, Normalise a) => a -> C (Reduced a)
    mkReduced t = do
      [simplified, reduced, normalised] <-
        mapM (withTimeout t) [simplify t, reduce t, normalise t]
      return $ Reduced {original = t, ..}

    reportReduced :: PrettyTCM a => Reduced a -> C ()
    reportReduced Reduced{..} = do
      report 30 $ "  *simplified: " <> ppm simplified
      report 30 $ "     *reduced: " <> ppm reduced
      report 30 $ "  *normalised: " <> ppm normalised

-- Run the training function on each subterm of a definition.
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
    ignoreType    = any cubicalRelated . map pp . names
    ignoreCtxType = any cubicalRelated . map pp . names

    cubicalRelated, tooSlow :: String -> Bool
    cubicalRelated = ("Agda.Primitive.Cubical.I" `isInfixOf`)
    tooSlow        = ("Data.Rational.Properties" `isPrefixOf`)
                  \/ ("Prelude.Solvers" `isPrefixOf`)

    go :: Type -> Term -> C ()
    go ty t = whenM (not <$> ignore ty)
            $ silently (checkInternal' act t CmpLeq ty)

    act :: Action C
    act = defaultAction {preAction = pre}

    pre :: Type -> Term -> C Term
    pre ty t = trainF ty t >> return t

-- ** Gathering names from terms

names :: TermLike a => a -> [QName]
names = foldTerm $ \case
  (Def n _) -> [n]
  (Con c _ _) -> [conName c]
  _ -> []

-- ** Utilities

ppm :: PrettyTCM a => a -> TCM Doc
ppm = prettyTCM

reportTCM = reportSDoc "toTrain"
report k  = liftTCM . reportTCM k

(\/) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(f \/ g) x = f x || g x

noop :: C ()
noop = return ()

silently :: C a -> C ()
silently k = void k `catchError` \ _ -> noop

maxDuration = 2 -- seconds

withTimeout :: a -> C a -> C a
withTimeout t k = getTC >>= \ s -> liftIO $
  caseEitherM
    (race (threadDelay (maxDuration * 1000000))
          (fst <$> runSafeTCM (runC k) s))
    (\() -> return t)
    return
