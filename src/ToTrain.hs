module ToTrain where

import Data.Maybe ( fromJust )
import Data.List ( nub, isInfixOf )
import qualified Data.Set as S

import Control.Monad ( forM_, void )

import Agda.Syntax.Common
import Agda.Syntax.Internal
import Agda.Syntax.Internal.Generic

import Agda.Utils.Pretty ( prettyShow )
import Agda.Utils.Monad ( whenM )

import Agda.TypeChecking.CheckInternal

import Agda.TypeChecking.Monad
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.CheckInternal

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
  report $ "ctx: " <> prettyTCM ctx
  report $ "type: " <> prettyTCM ty
  report $ "term: " <> prettyTCM t
  report $ "names: " <> prettyTCM (names t)
  report "}"

-- Run the training function on each subterm of a definition.
forEachHole :: TrainF -> Definition -> TCM ()
forEachHole k def@(Defn{..}) = do
  report $ "------ definition: " <> prettyTCM defName <> " -------"
  let
      ignoreType, ignoreCtxType :: Type -> Bool
      ignoreType    = any cubicalRelated . map prettyShow . names
      ignoreCtxType = any cubicalRelated . map prettyShow . names

      ignore :: Type -> TCM Bool
      ignore ty = do
        ctx <- fmap (snd . unDom) <$> getContext
        return (ignoreType ty || any ignoreCtxType ctx)

      act :: Action TCM
      act = defaultAction {postAction = \ty t -> do
        whenM (not <$> ignore ty) $
          train ty t
        return t
        }

      go :: Type -> Term -> TCM ()
      go ty t = do
        whenM (not <$> ignore ty) $
          void $ checkInternal' act t CmpLeq ty
  case theDef of
    (Function{..}) -> forM_ funClauses $ \c@(Clause{..}) -> addContext clauseTel $
      case (clauseBody, unArg <$> clauseType) of
        (Just t, Just ty) -> train ty t >> go ty t
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

cubicalRelated :: String -> Bool
cubicalRelated = ("Agda.Primitive.Cubical.I" `isInfixOf`)
