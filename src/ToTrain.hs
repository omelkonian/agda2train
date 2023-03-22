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
import Agda.Syntax.Scope.Base ( nsInScope, allThingsInScope, inverseScopeLookupName )
import Agda.Syntax.Scope.Monad ( getCurrentScope )
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

report     = reportSDoc "toTrain" 1
debugPrint = reportSDoc "toTrain" 10
noop       = return ()

-- A training function generates training data for each typed sub-term,
-- with access to the local context via the typechecking monad.
type TrainF = Type -> Term -> TCM ()

-- an example training function that just prints the relevant (local) information
train :: TrainF
train ty t = do
  ctx <- getContextTelescope
  let ns = names' t
  unless (null ns) $
    whenM (S.isSubsetOf ns . nsInScope . allThingsInScope <$> getCurrentScope) $ do
      report "{"
      report $ " ctx: " <> prettyTCM (prettyShow ctx)
      debugPrint $ "   pp:" <> prettyTCM ctx
      report $ " goal: " <> prettyTCM (prettyShow ty)
      debugPrint $ "   pp:" <> prettyTCM ty
      report $ " term: " <> prettyTCM (prettyShow t)
      debugPrint $ "   names: " <> prettyTCM (prettyShow ns)
      debugPrint $ "   pp: " <> prettyTCM t
      report "}"


-- Run the training function on each subterm of a definition.
forEachHole :: TrainF -> Definition -> TCM ()
forEachHole k def@Defn{..} = {-unless (null $ getRange defName) $-} do
  debugPrint $ "------ definition: " <> prettyTCM defName <> " -------"
  -- Defn{..} <- instantiateDef def
  sc <- getScope
  unless (ignoreDef def) $ case theDef of
    (Function{..}) ->
      -- withPragmaOptions (\o -> o { optProjectionLike = False }) $
      forM_ funClauses $ \c@(Clause{..}) -> addContext clauseTel $
        case (clauseBody, unArg <$> clauseType) of
          (Just t, Just ty) -> go ty t
          _ -> debugPrint "no clause body" >> noop
    _ -> debugPrint "not a function" >> noop
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
            $ checkInternal' act t CmpLeq ty
            -- $ reconstructParameters' act ty t

    act :: Action TCM
    act = defaultAction {preAction = pre}

    pre :: Type -> Term -> TCM Term
    pre ty t = train ty t >> return t

-- ** Gathering names from terms
names' :: TermLike a => a -> S.Set QName
names' = foldTerm $ \case
  (Def n _) -> S.singleton n
  (Con c _ _) -> S.singleton (conName c)
  _ -> S.empty

names :: TermLike a => a -> [QName]
names = S.toList . names'
