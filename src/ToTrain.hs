module ToTrain
  ( TrainF
  , train
  , forEachHole
  , runC
  , mkReduced
  , reportReduced
  ) where

import Data.Maybe ( isJust )
import Data.List ( isPrefixOf, isInfixOf, find )
import qualified Data.Set as S

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
import Output

-- ** Extending the typechecking monad to also record/output training samples.
type C = WriterT [Sample] TCM

runC :: C () -> TCM [Sample]
runC = (snd <$>) . runWriterT

noop :: C ()
noop = return ()

silently :: C a -> C ()
silently k = void k `catchError` \ _ -> noop

-- | A training function generates training data for each typed sub-term,
-- with access to the local context via the typechecking monad.
type TrainF = Type -> Term -> C ()

-- | An example training function that just prints the relevant (local) information.
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

      ctx' <- liftTCM $ convert ctx
      rty' <- liftTCM $ traverse convert rty
      rt'  <- liftTCM $ traverse convert rt
      tell1 $ Sample
        { ctx      = prender pctx :> ctx'
        , goal     = prender pty  :> rty'
        , term     = prender pt   :> rt'
        , premises = map pp ns
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
    ignoreType    = any cubicalRelated . map pp . names
    ignoreCtxType = any cubicalRelated . map pp . names

    cubicalRelated, tooSlow :: String -> Bool
    cubicalRelated = ("Agda.Primitive.Cubical.I" `isInfixOf`)
    tooSlow
       = ("Data.Rational.Properties" `isPrefixOf`)
      \/ ("Prelude.Solvers" `isPrefixOf`)
      \/ ("foundation.partitions" `isPrefixOf`)
      \/ (== "Data.Relation.Binary.Subset.Propositional.Properties.filter⁺")
      \/ (== "foundation.homotopies._.compute-ind-htpy")
      \/ (== "foundation.equivalences.coh-unit-laws-equiv")
      \/ (== "foundation.functoriality-propositional-truncation.id-map-trunc-Prop")
      \/ (== "foundation.functoriality-coproduct-types._.equiv-mutually-exclusive-coprod")
      \/ (== "trees.polynomial-endofunctors.coh-refl-htpy-polynomial-endofunctor")
      \/ (== "foundation.uniqueness-image._.htpy-map-hom-equiv-slice-uniqueness-im")
      \/ (== "foundation.uniqueness-image._.tetrahedron-hom-equiv-slice-uniqueness-im")
      \/ (== "foundation.functoriality-set-truncation._.htpy-map-hom-slice-trunc-im-Set")
      \/ (== "foundation.functoriality-set-truncation._.tetrahedron-map-hom-slice-trunc-im-Set")
      \/ (== "univalent-combinatorics.2-element-types._.is-not-identity-swap-2-Element-Type")
      \/ (== "foundation.unordered-pairs._.eq-Eq-refl-unordered-pair")
      \/ (== "MGS.Equivalence-Induction.𝔾-≃-equation")
      \/ (== "MGS.More-FunExt-Consequences.EM-is-subsingleton")
      \/ (== "foundation.unordered-pairs.preserves-refl-htpy-unordered-pair")
      \/ (== "MGS.Equivalence-Constructions.Eq-Eq-cong")
      \/ (== "foundation.unordered-pairs.id-equiv-standard-unordered-pair")
      \/ (== "MGS.Embeddings.Emb→fun")
      \/ (== "trees.morphisms-algebras-polynomial-endofunctors._.refl-htpy-hom-algebra-polynomial-endofunctor")
      \/ (== "MGS.Universe-Lifting._.q")
      \/ (== "UF.Equiv-FunExt.prop-＝-≃-⇔")
      \/ (== "trees.morphisms-algebras-polynomial-endofunctors._.is-contr-total-htpy-hom-algebra-polynomial-endofunctor")
      \/ (== "trees.w-types.htpy-hom-𝕎-Alg")
      \/ (== "UF.FunExt-Properties.naive-funext-gives-funext₀")
      \/ (== "graph-theory.morphisms-undirected-graphs._.refl-htpy-hom-Undirected-Graph")
      \/ (== "UF.UA-FunExt._._.h")
      \/ (== "graph-theory.morphisms-undirected-graphs._.is-contr-total-htpy-hom-Undirected-Graph")
      \/ (== "UF.PairFun.pair-fun-embedding")
      \/ (== "graph-theory.equivalences-undirected-graphs._.edge-standard-unordered-pair-vertices-id-equiv-Undirected-Graph")
      \/ (== "UF.UniverseEmbedding.hSet-embeddings-are-embeddings")
      \/ (== "graph-theory.equivalences-undirected-graphs._.is-contr-total-htpy-equiv-Undirected-Graph")
      \/ (== "UF.Size._.Set-Replacement")
      \/ (== "Lifting.IdentityViaSIP.𝓛-Id")
      \/ (== "foundation.partitions._.is-section-map-inv-compute-block-partition")
      \/ (== "Lifting.IdentityViaSIP.𝓛-Id·")
      \/ (== "foundation.partitions._.is-retraction-map-inv-compute-block-partition")
      \/ (== "Lifting.Size._.e")
      \/ (== "foundation.partitions._.is-emb-inhabited-subtype-block-partition")
      \/ (== "InjectiveTypes.Blackboard.injective._.e")
      \/ (== "foundation.partitions._.compute-is-in-block-partition")
      \/ (== "foundation.partitions._.is-contr-block-containing-element-partition")
      \/ (== "foundation.partitions._.is-in-block-class-partition")
      \/ (== "foundation.partitions._.compute-total-block-partition")
      \/ (== "TypeTopology.SquashedCantor._.fg")
      \/ (== "foundation.partitions.partition-Set-Indexed-Σ-Decomposition")
      \/ (== "foundation.set-quotients._.is-section-equivalence-class-set-quotient") -- really hungry
      \/ (== "foundation.set-quotients._.is-retraction-equivalence-class-set-quotient")
      -- \/ (== "")


    go :: Type -> Term -> C ()
    go ty t = whenM (not <$> ignore ty)
            $ silently (checkInternal' act t CmpLeq ty)

    act :: Action C
    act = defaultAction {preAction = pre}

    pre :: Type -> Term -> C Term
    pre ty t = trainF ty t >> return t

-- | Gathering names from terms.
names :: TermLike a => a -> [QName]
names = foldTerm $ \case
  (Def n _) -> [n]
  (Con c _ _) -> [conName c]
  _ -> []

-- ** reduction

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
  xs <- mapM (liftTCM . withTimeout) [simplify t, reduce t, normalise t]
  -- compress Reduced representation (erase identical terms)
  let (simplified : ys) = find (/= t)                   <$> xs
      (reduced    : zs) = find ((/= simplified) . Just) <$> ys
      [normalised]      = find ((/= reduced)    . Just) <$> zs
  return $ Reduced {original = t, ..}

reportReduced :: (MonadTCM m, PrettyTCM a) => Reduced a -> m ()
reportReduced Reduced{..} = do
  report 30 $ "  *simplified: " <> ppm simplified
  report 30 $ "     *reduced: " <> ppm reduced
  report 30 $ "  *normalised: " <> ppm normalised
