module ToTrain where

import Data.Maybe ( fromJust )
import Data.List ( nub, isInfixOf )
import qualified Data.Set as S

import Control.Monad ( forM_, void, when, unless )

import Agda.Syntax.Common
import Agda.Syntax.Abstract.Name ( QName(..) )
import Agda.Syntax.Internal
import Agda.Syntax.Internal.Generic
import Agda.Syntax.Position ( getRange )
import Agda.Syntax.Scope.Base ( inverseScopeLookupName )

import Agda.Utils.Pretty ( prettyShow )
import Agda.Utils.Monad ( whenM, unlessM )

import Agda.TypeChecking.CheckInternal

import Agda.TypeChecking.Monad
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.CheckInternal
import Agda.TypeChecking.ProjectionLike ( ProjEliminator(..), elimView )
import Agda.TypeChecking.ReconstructParameters ( reconstruct )

report = reportSDoc "toTrain" 1
noop   = return ()

-- A training function generates training data for each typed sub-term,
-- with access to the local context via the typechecking monad.
type TrainF = Type -> Term -> TCM ()

-- an example training function that just prints the relevant (local) information
train :: TrainF
train ty t = do
  ctx <- getContextTelescope
  report "{"
  -- report $ "ctx: " <> prettyTCM ctx
  report $ "type: " <> prettyTCM ty
  report $ "term: " <> prettyTCM t
  -- report $ "names: " <> prettyTCM (names t)
  report "}"

-- Run the training function on each subterm of a definition.
forEachHole :: TrainF -> Definition -> TCM ()
forEachHole k def@(Defn{..}) = do -- unless (null $ getRange defName) $ do
 sc <- getScope
 unless (null $ inverseScopeLookupName defName sc) $ do
  report $ "------ definition: " <> prettyTCM (prettyShow defName) <> " -------"
  let
      cubicalRelated :: String -> Bool
      cubicalRelated = ("Agda.Primitive.Cubical.I" `isInfixOf`)

      ignoreType, ignoreCtxType :: Type -> Bool
      ignoreType    = any cubicalRelated . map prettyShow . names
      ignoreCtxType = any cubicalRelated . map prettyShow . names

      ignore :: Type -> TCM Bool
      ignore ty = do
        ctx <- fmap (snd . unDom) <$> getContext
        return (ignoreType ty || any ignoreCtxType ctx)

      act :: Action TCM
      act = defaultAction
        { postAction = \ty t -> do
            whenM (not <$> ignore ty) $
              train ty t
            return t
        , elimViewAction = \t -> do
            -- report "****** elimView ******"
            -- report $ "  t: " <> prettyTCM (prettyShow t)
            t' <-
              return t
              -- elimView EvenLone t
              -- elimView ButLone t
              -- elimView NoPostfix t
            -- report $ " t': " <> prettyTCM (prettyShow t')
            -- report "**********************"
            return t'
        }

      go :: Type -> Term -> TCM ()
      go ty t = whenM (not <$> ignore ty) $ void (checkInternal' act t CmpLeq ty)
  -- unless (isAnonymousModuleName $ qnameModule defName) $
  case theDef of
      (Function{..}) -> forM_ funClauses $ \c@(Clause{..}) -> addContext clauseTel $
        case (clauseBody, unArg <$> clauseType) of
          (Just t, Just ty) -> do
            train ty t
            -- t <- reconstruct ty t
            -- ctx <- getContextTelescope
            -- report $ "ctx: " <> prettyTCM ctx
            go ty t
          _ -> report "no clause body" >> noop
      _ -> report "not a function" >> noop

-- ** Gathering names from terms
names' :: TermLike a => a -> S.Set QName
names' = foldTerm $ \case
  (Def n _) -> S.singleton n
  (Con c _ _) -> S.singleton (conName c)
  _ -> S.empty

names :: TermLike a => a -> [QName]
names = S.toList . names'
