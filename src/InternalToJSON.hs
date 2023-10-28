{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeApplications #-}
-- | Conversion from Agda's internal syntax to our simplified JSON format.
module InternalToJSON where

import Control.Monad.Reader ( ReaderT, asks, liftIO )

import Data.List ( notElem, elemIndex )
import Data.Maybe ( maybeToList, fromMaybe )
import qualified Data.IntMap as M

import qualified Agda.Utils.VarSet as VS
import Agda.Utils.Monad ( ifM )
import Agda.Syntax.Common ( unArg )
import qualified Agda.Syntax.Common as A
import qualified Agda.Syntax.Internal as A
import qualified Agda.Syntax.Literal as A
import Agda.Syntax.Internal
  ( QName, absName, qnameName, qnameModule, unAbs, unEl, unDom
  , nameId, conName, dbPatVarIndex, pDom, telToList, telFromList )
import Agda.Syntax.Translation.InternalToAbstract ( NamedClause(..) )
import qualified Agda.TypeChecking.Monad as A
import Agda.TypeChecking.Monad
  ( TCM, MonadTCM, liftTCM, typeOfConst, theDef, defName, getConstInfo
  , reportSDoc, VerboseLevel )
import Agda.TypeChecking.Free ( freeVars, VarCounts(..) )
import Agda.TypeChecking.Level ( isLevelType )

import qualified Agda.Utils.Pretty as P
import Agda.TypeChecking.Pretty
  ( PrettyTCM(..), MonadPretty, fsep, punctuate, braces, parens, Doc )
import qualified Agda.TypeChecking.Pretty as P
  hiding (text)

import AgdaInternals ()
import Options
import JSON

-- | Wrapper around Agda's typechecking monad that also provides user options.
type TCM_WithOpts = ReaderT Options TCM

-- | Converting between two types @a@ and @b@ under Agda's typechecking monad.
--
-- NB: 'go' is only used internally to de-clutter the recursive calls.
class (~>) a b | a -> b where
  convert, go :: a -> TCM_WithOpts b
  convert = go

instance A.Definition ~> Definition where
  go = go . theDef

instance A.Defn ~> Definition where
  go = \case
    A.AbstractDefn defn -> go defn
    A.Function{..} -> let cls = takeWhile isNotCubical funClauses in
      -- NB: handle funWith and funExtLam
      Function <$> traverse go cls
    A.Datatype{..} -> do
      -- NB: what is a dataClause???
      tys <- traverse typeOfConst dataCons
      ADT <$> traverse go tys
    A.Record{..} -> do
      -- NB: incorporate conHead/namedCon in the future for accuracy
      --     + to solve the issue with private (non-public) fields
      (tel, fs) <- splitAt recPars <$> go recTel
      return $ Record tel (thing . item <$> fs)
    A.Constructor{..} -> do
      let cn = conName conSrcCon
      d <- theDef <$> getConstInfo conData
      return $ case d of
        A.Datatype{..} ->
          let Just ix = elemIndex (unqualify cn) (unqualify <$> dataCons)
          in  Constructor (pp conData) (toInteger ix)
        A.Record{..} -> Constructor (pp conData) 0
    A.Primitive{..}      -> return Primitive
    A.PrimitiveSort{..}  -> return Primitive
    A.Axiom{..}          -> return Postulate
    d@A.DataOrRecSig{..} -> panic "dataOrRecSig" d
    d@A.GeneralizableVar -> panic "generalizable variable" d

instance A.Clause ~> Clause where
  go A.Clause{..} =
    Clause <$> go clauseTel
           <*> traverse go (A.namedThing . unArg <$> namedClausePats)
           -- ^ drop visibility and name information
           <*> traverse go clauseBody

instance A.DeBruijnPattern ~> Pattern where
  go = \case
    A.VarP _ v -> return $ App (DB $ dbPatVarIndex v) []
    A.DotP _ t -> go t
    A.ConP c _ ps -> do
      App (Ref $ pp c) <$> traverse go (A.namedThing . unArg <$> ps)
    A.LitP _ lit -> return $ Lit (pp lit)
    A.ProjP _ qn -> return $ App (Ref $ pp qn) []
    p@(A.IApplyP _ _ _ _) -> panic "pattern (cubical)" p
    p@(A.DefP _ _ _)      -> panic "pattern (cubical)" p

instance A.Telescope ~> Telescope where
  go = traverse action . A.telToList
    where
    action :: A.Dom (Name, A.Type) -> TCM_WithOpts (Named (Pretty Type))
    action dty = do
      let (n, ty) = unDom dty
      pty <- ppm ty
      ty' <- go ty
      let pdty = prender $ pDom dty $ P.text $ n <> " : " <> prender pty
      return $ n :~ pdty :> ty'

instance A.Type ~> Type where
  go ty = do
    let t = A.unEl ty
    dl <- dependencyLevel t
    Type <$> go t <*> ifM (asks includeDepLvls) (pure $ Just dl) (pure Nothing)

instance A.Term ~> Term where
  go = flip (.) A.unSpine $ \case
    -- ** abstractions
    (A.Pi ty ab) -> do
      ty' <- go (unEl $ unDom ty)
      ab' <- go (unEl $ unAbs ab)
      return $ Pi (isDependentArrow ty) (absName ab :~ ty') ab'
    (A.Lam _ ab) -> do
      ab' <- go (unAbs ab)
      return $ Lam (pp (absName ab) :~ ab')
    -- ** applications
    (A.Var i   xs) -> App (DB i)                     <$> (traverse go xs)
    (A.Def f   xs) -> App (Ref $ ppName f)           <$> (traverse go xs)
    (A.Con c _ xs) -> App (Ref $ ppName $ conName c) <$> (traverse go xs)
    -- ** other constants
    (A.Lit   x)   -> return $ Lit   $ pp x
    (A.Level x)   -> return $ Level $ pp x
    (A.Sort  x)   -> return $ Sort  $ pp x
    (A.MetaV _ _) -> return UnsolvedMeta
    -- ** there are some occurrences of `DontCare` in the standard library
    (A.DontCare t) -> go t
    -- ** crash on the rest (should never be encountered)
    t@(A.Dummy _ _) -> panic "term" t

instance A.Elim ~> Term where
  go = \case
    (A.Apply x)      -> go (unArg x)
    (A.Proj _ qn)    -> return $ App (Ref $ ppName qn) []
    (A.IApply _ _ x) -> go x

-- | Calculating a type's "dependency level" (c.f. Issue #11).
dependencyLevel :: A.Term -> TCM_WithOpts Int
dependencyLevel = go
  where
  go :: A.Term -> TCM_WithOpts Int
  go = \case
    (A.Pi ty ab) -> do
      isLvl <- isLevelType (unDom ty)
      let ab' = unEl $ unAbs ab
          fvs = fromMaybe 0 $ varCounts (freeVars @() @VarCounts @A.Term ab') M.!? 0
          n   = if isLvl || not (isDependentArrow ty) then 0 else fvs
      (n +) <$> go ab'
    (A.Lam _ ab)   -> go (unAbs ab)
    (A.Def _ xs)   -> sum <$> mapM goE xs
    (A.Con _ _ xs) -> sum <$> mapM goE xs
    _ -> return 0

  goE :: A.Elim -> TCM_WithOpts Int
  goE = \case
    (A.Apply a) -> go (unArg a)
    _ -> return 0

-- * Utilities

pp :: P.Pretty a => a -> String
pp = P.prettyShow

ppm :: (MonadPretty m, P.PrettyTCM a) => a -> m Doc
ppm = P.prettyTCM

prender :: Doc -> String
prender = P.renderStyle (P.Style P.OneLineMode 0 0.0)

pinterleave :: (Applicative m, Semigroup (m Doc)) => m Doc -> [m Doc] -> m Doc
pinterleave sep = fsep . punctuate sep

pbindings :: (MonadPretty m, PrettyTCM a) => [(String, a)] -> [m Doc]
pbindings = map $ \(n, ty) -> parens $ ppm n <> " : " <> ppm ty

report :: MonadTCM m => VerboseLevel -> TCM Doc -> m ()
report n x = liftTCM $ reportSDoc "agda2train" n x

panic :: (P.Pretty a, Show a) => String -> a -> b
panic s t = error $
  "[PANIC] unexpected " <> s <> ": " <> pp t <> "\n show: " <> pp (show t)

ppName :: A.QName -> String
ppName qn = pp qn <> "<" <> show (fromEnum $ nameId $ qnameName qn) <> ">"

unqualify :: A.QName -> String
unqualify = pp . qnameName

(\/) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(f \/ g) x = f x || g x

isNotCubical :: A.Clause -> Bool
isNotCubical A.Clause{..}
  | Just (A.Def qn _) <- clauseBody
  = pp (qnameModule qn) /= "Agda.Primitive.Cubical"
  | otherwise
  = True

isDependentArrow :: A.Dom A.Type -> Bool
isDependentArrow ty = pp (A.domName ty) `notElem` ["_", "(nothing)"]

instance P.PrettyTCM A.Definition where
  prettyTCM d = go (theDef d)
   where
    go = \case
      A.AbstractDefn defn -> go defn
      A.Function{..} -> let cls = takeWhile isNotCubical funClauses in
        fsep $ punctuate " |"
             $ ppm . NamedClause (defName d) True <$> cls
      A.Datatype{..} -> do
        tys <- fmap unEl <$> traverse typeOfConst dataCons
        pinterleave " |" $ pbindings $ zip (unqualify <$> dataCons) tys
      A.Record{..} ->
        let (tel, fs) = splitAt recPars $ telToList recTel in
        (if null tel then "" else ppm (telFromList tel) <> " |- ")
          <> (braces $ pinterleave " ;" $ pbindings $ unDom <$> fs)
      A.Constructor{..} -> do
        let cn = conName conSrcCon
        d <- theDef <$> getConstInfo conData
        case d of
          A.Datatype{..} ->
            let Just ix = elemIndex (unqualify cn) (unqualify <$> dataCons)
            in  ppm conData <> "@" <> ppm ix
          A.Record{..} -> ppm conData <> "@0"
      A.Primitive{..}     -> "<Primitive>"
      A.PrimitiveSort{..} -> "<PrimitiveSort>"
      A.Axiom{..}         -> "<Axiom>"
      A.DataOrRecSig{..}  -> "<DataOrRecSig>"
      A.GeneralizableVar  -> "<GeneralizableVar>"


