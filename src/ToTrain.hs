module ToTrain where

import Data.Maybe ( fromJust )
import Data.List ( nub, isPrefixOf, isInfixOf )
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
import Agda.TypeChecking.ProjectionLike
  ( ProjEliminator(..), elimView, reduceProjectionLike )
import Agda.TypeChecking.ReconstructParameters
  ( reconstruct, dropParameters, reconstructParameters' )

import Agda.Interaction.Options ( optProjectionLike )

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
  report $ " ctx: " <> prettyTCM ctx
  report $ "  db:" <> prettyTCM (prettyShow ctx)
  report $ " type: " <> prettyTCM ty
  report $ "  db:" <> prettyTCM (prettyShow ty)
  report $ " term: " <> prettyTCM t
  report $ "  db:" <> prettyTCM (prettyShow t)
  -- report $ " elims: " <> prettyTCM (elims t)
  report "}"
  where
    elims :: Term -> Elims
    elims = \case
      Def _ es -> es
      Var _ es -> es
      Con _ _ es -> es
      _ -> []

-- Run the training function on each subterm of a definition.
forEachHole :: TrainF -> Definition -> TCM ()
forEachHole k def@Defn{..} = {-unless (null $ getRange defName) $-} do
  report $ "------ definition: " <> prettyTCM defName <> " -------"
  -- Defn{..} <- instantiateDef def
  sc <- getScope
  unless (ignoreDef def) $ case theDef of
    (Function{..}) ->
      -- withPragmaOptions (\o -> o { optProjectionLike = False }) $
      forM_ funClauses $ \c@(Clause{..}) -> addContext clauseTel $
        case (clauseBody, unArg <$> clauseType) of
          (Just t, Just ty) -> do
            -- train ty t
            -- t <- reconstruct ty t
            -- ctx <- getContextTelescope
            -- report $ "ctx: " <> prettyTCM ctx
            go ty t
          _ -> report "no clause body" >> noop
    _ -> report "not a function" >> noop
  where
    ignoreDef :: Definition -> Bool
    ignoreDef Defn{..}
       = False
      -- || defCopy
      -- || defNoCompilation
      -- || null (inverseScopeLookupName defName sc)
      -- || isAnonymousModuleName (qnameModule defName)
      -- || ("._." `isInfixOf` prettyShow defName)
      -- || (getOrigin defName == UserWritten)
      || ( ("with-" `isPrefixOf` prettyShow (qnameName defName))
        && ("Data.Record" `isPrefixOf` prettyShow (qnameModule defName))
         )
      -- || "Binary.Reasoning.Setoid.Base" `isInfixOf` prettyShow defName

    ignore :: Type -> TCM Bool
    ignore ty = do
      ctx <- fmap (snd . unDom) <$> getContext
      return (ignoreType ty || any ignoreCtxType ctx)

    ignoreType, ignoreCtxType :: Type -> Bool
    ignoreType    = any cubicalRelated . map prettyShow . names
    ignoreCtxType = any cubicalRelated . map prettyShow . names

    cubicalRelated :: String -> Bool
    cubicalRelated = ("Agda.Primitive.Cubical.I" `isInfixOf`)

    go :: Type -> Term -> TCM ()
    go ty t = whenM (not <$> ignore ty)
            $ void
            -- $ checkInternal' act t CmpLeq ty
            $ reconstructParameters' act ty t

    act :: Action TCM
    act = defaultAction {preAction = pre, postAction = post}

    pre, post :: Type -> Term -> TCM Term
    pre  ty t =
      -- unlessM (ignore ty) $
        train ty t >> return t
    post ty t =
      -- unlessM (ignore ty) $
        -- reconstruct ty t
        reduceProjectionLike t
          -- return t
      -- train ty t >> return t

    {-elim :: Term -> TCM Term
    elim t = do
      report "****** elimView ******"
      report $ " t: " <> prettyTCM (prettyShow t)
      report $ " elims: " <> prettyTCM (elims t)
      t' <-
        -- return t
        elimView EvenLone t
        -- elimView ButLone t
        -- elimView NoPostfix t
      report $ " t': " <> prettyTCM (prettyShow t')
      report $ " elims: " <> prettyTCM (elims t')
      report "**********************"
      return t'-}

-- ** Gathering names from terms
names' :: TermLike a => a -> S.Set QName
names' = foldTerm $ \case
  (Def n _) -> S.singleton n
  (Con c _ _) -> S.singleton (conName c)
  _ -> S.empty

names :: TermLike a => a -> [QName]
names = S.toList . names'



