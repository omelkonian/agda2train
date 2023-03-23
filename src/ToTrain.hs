module ToTrain where

import Data.Maybe ( fromJust )
import Data.List ( nub, isPrefixOf, isInfixOf )
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BS
import Data.String

import Control.Monad ( forM_, void, when, unless )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Writer -- ( WriterT, runWriterT, tell )
import Control.Monad.State

import Text.PrettyPrint ( render )

import Agda.Syntax.Common
import Agda.Syntax.Abstract.Name ( QName(..) )
import Agda.Syntax.Internal
import Agda.Syntax.Internal.Generic
import Agda.Syntax.Position ( getRange )
import Agda.Syntax.Scope.Base ( nsInScope, allThingsInScope, inverseScopeLookupName )
import Agda.Syntax.Scope.Monad ( getCurrentScope )
import Agda.Utils.Monad ( whenM, unlessM )

import Agda.TypeChecking.CheckInternal

import Agda.TypeChecking.Monad
import Agda.TypeChecking.Warnings ( MonadWarning )
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.CheckInternal
import Agda.TypeChecking.ProjectionLike
  ( ProjEliminator(..), elimView, reduceProjectionLike )
import Agda.TypeChecking.ReconstructParameters
  ( reconstruct, dropParameters, reconstructParameters' )

import Agda.Interaction.Options ( optProjectionLike )

import Output
import Data.Aeson ( encode )

import AgdaInternals

ppm :: PrettyTCM a => a -> TCM Doc
ppm = prettyTCM
reportTCM = reportSDoc "toTrain" 1
report    = liftTCM . reportTCM

type C = WriterT [Sample] TCM

noop :: C ()
noop = return ()

runC :: C () -> TCM [Sample]
runC = (snd <$>) . runWriterT

-- A training function generates training data for each typed sub-term,
-- with access to the local context via the typechecking monad.
type TrainF = Type -> Term -> C ()

-- an example training function that just prints the relevant (local) information
train :: TrainF
train ty t = do
  ctx <- liftTCM getContextTelescope
  let ns = names t
  sc <- liftTCM getCurrentScope
  unless (null ns) $
    when (S.fromList ns `S.isSubsetOf` nsInScope (allThingsInScope sc)) $ do
      ctx <- getContextTelescope
      pctx <- liftTCM $ ppm ctx
      pty  <- liftTCM $ ppm ty
      pt   <- liftTCM $ ppm t
      tell [Sample
        { ctx  = (render pctx, convert ctx)
        , goal = (render pty,  convert ty)
        , term = (render pt,   convert t)
        , namesUsed = map pp ns
        }]
      report "{"
      report $ " ctx: " <> ppm (pp ctx)
      report $ "   pp:" <> ppm ctx
      report $ " goal: " <> ppm (pp ty)
      report $ "   pp:" <> ppm ty
      report $ " term: " <> ppm (pp t)
      report $ "   names: " <> ppm ns
      report $ "   pp: " <> ppm t
      report "}"

-- Run the training function on each subterm of a definition.
forEachHole :: TrainF -> Definition -> C ()
forEachHole k def@Defn{..} = do
  report $ "------ definition: " <> ppm defName <> " -------"
  sc <- getScope
  unless (ignoreDef def) $ case theDef of
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
      -- || defCopy
      -- || defNoCompilation
      -- || null (inverseScopeLookupName defName sc)
      -- || isAnonymousModuleName (qnameModule defName)
      -- || ("._." `isInfixOf` pp defName)
      -- || (getOrigin defName == UserWritten)
      -- || ( ("with-" `isPrefixOf` pp (qnameName defName))
      --   && ("Data.Record" `isPrefixOf` pp (qnameModule defName))
      --    )
      -- || "Binary.Reasoning.Setoid.Base" `isInfixOf` pp defName

    ignore :: Type -> C Bool
    ignore ty = do
      ctx <- fmap (snd . unDom) <$> getContext
      return (ignoreType ty || any ignoreCtxType ctx)

    ignoreType, ignoreCtxType :: Type -> Bool
    ignoreType    = any cubicalRelated . map pp . names
    ignoreCtxType = any cubicalRelated . map pp . names

    cubicalRelated :: String -> Bool
    cubicalRelated = ("Agda.Primitive.Cubical.I" `isInfixOf`)

    go :: Type -> Term -> C ()
    go ty t = whenM (not <$> ignore ty)
            $ void
            $ checkInternal' act t CmpLeq ty

    act :: Action C
    act = defaultAction {preAction = pre}

    pre :: Type -> Term -> C Term
    pre ty t = train ty t >> return t

-- ** Gathering names from terms
names :: TermLike a => a -> [QName]
names = foldTerm $ \case
  (Def n _) -> [n]
  (Con c _ _) -> [conName c]
  _ -> []
