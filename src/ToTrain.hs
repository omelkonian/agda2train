module ToTrain
  ( TrainF
  , train
  , forEachHole
  , runC
  , mkReduced
  , report, reportReduced
  , ppm
  , toTerm
  ) where

import Data.List ( isPrefixOf, isInfixOf, find, elemIndex )
import qualified Data.Set as S

import Control.Monad ( forM_, void, when, unless )
import Control.Monad.Writer ( WriterT(runWriterT) )
import Control.Monad.Error.Class ( catchError )
import Control.Monad.IO.Class ( liftIO )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async ( race )

import Text.PrettyPrint ( render )

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
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.CheckInternal
  ( Action(..), defaultAction, checkInternal' )

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
      \/ (== "Data.Relation.Binary.Subset.Propositional.Properties.filter⁺")

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

-- | Embedding definitions into terms (over-approximation)

class ToTerm a where
  toTerm :: a -> TCM Term

instance ToTerm Definition where
  toTerm = toTerm . theDef

instance ToTerm Defn where
  toTerm = \case
    AbstractDefn defn -> toTerm defn
    Function{..} ->
    -- NB: handle funWith and funExtLam
      combineClauses <$> traverse toTerm funClauses
    Datatype{..} -> do
    -- NB: what is a dataClause???
      tys <- traverse typeOfConst dataCons
      pure $ combineConstructors (unEl <$> tys)
    Record{..} -> do
    -- NB: what is a recClause???
    -- NB: maybe incorporate conHead/namedCon in the future for accuracy
      tys <- traverse typeOfConst (unDom <$> recFields)
      pure $ combineFields (unEl <$> tys)
    Constructor{..} -> do
      let cn = conName conSrcCon
      d <- theDef <$> getConstInfo conData
      pure $ case d of
        Datatype{..} ->
          let Just ix = elemIndex (unqualify cn) (unqualify <$> dataCons)
          in  mkConDef conData (toInteger ix)
        Record{..} ->
          mkConDef conData 0
    Primitive{..}     -> pure $ Dummy ("$" <> primName) []
    PrimitiveSort{..} -> pure $ Dummy ("$" <> primSortName) []
    Axiom{..}         -> pure dummyTop
    DataOrRecSig{..}  -> pure dummyTop
    GeneralizableVar  -> pure dummyTop

instance ToTerm Clause where
  toTerm Clause{..} = pure $ case clauseBody of
    Just t  -> mkLam (namedArg <$> namedClausePats) t
    Nothing -> dummyBot

mkLam :: [DeBruijnPattern] -> (Term -> Term)
mkLam = go . concatMap varsIn
  where
    go :: [String] -> (Term -> Term)
    go []       = id
    go (x : xs) = Lam defaultArgInfo . Abs (show x) . go xs

    varsIn :: DeBruijnPattern -> [String]
    varsIn = \case
      VarP p _ -> varsInPatInfo p
      DotP p _ -> varsInPatInfo p
      ConP _ (ConPatternInfo{..}) ps ->
        varsInPatInfo conPInfo <> concatMap varsIn (namedArg <$> ps)
      LitP p _ -> varsInPatInfo p
      ProjP _ _ -> []
      IApplyP p _ _ _ -> varsInPatInfo p
      DefP p _ ps -> varsInPatInfo p <> concatMap varsIn (namedArg <$> ps)

    varsInPatInfo :: PatternInfo -> [String]
    varsInPatInfo PatternInfo{..} = case patOrigin of
      (PatOVar n) -> [nameToArgName n]
      _           -> []

combineTerms :: String -> [Term] -> Term
combineTerms s []  = dummyBot
combineTerms s [a] = a
combineTerms s as  = Dummy s (Apply . defaultArg <$> as)

combineClauses      = combineTerms "⊜"
combineConstructors = combineTerms "⊕"
combineFields       = combineTerms "⊗"

mkConDef :: QName -> Integer -> Term
mkConDef dty ix = combineTerms "⊙" [Def dty [], Lit (LitNat ix)]

dummyBot, dummyTop :: Term
dummyBot = Dummy "⊥" []
dummyTop = Dummy "⊤" []

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
  -- compress Reduced representation (erase idential terms)
  let [simplified, reduced, normalised] = find (/= t) <$> xs
  return $ Reduced {original = t, ..}

reportReduced :: (MonadTCM m, PrettyTCM a) => Reduced a -> m ()
reportReduced Reduced{..} = do
  report 30 $ "  *simplified: " <> ppm simplified
  report 30 $ "     *reduced: " <> ppm reduced
  report 30 $ "  *normalised: " <> ppm normalised

-- ** utilities

ppm :: PrettyTCM a => a -> TCM Doc
ppm = prettyTCM

report :: MonadTCM m => VerboseLevel -> TCM Doc -> m ()
report n x = liftTCM $ reportSDoc "agda2train" n x

(\/) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(f \/ g) x = f x || g x

unqualify :: QName -> String
unqualify = pp . qnameName
