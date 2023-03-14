-- {-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module ToTrain where

import Data.Maybe ( fromJust )
import Data.Generics ( listify )
import qualified Data.Set as S

import Control.Monad ( forM_ )

-- import Agda.Compiler.Common

import Agda.Syntax.Common
import Agda.Syntax.Internal

import Agda.TypeChecking.Monad
-- import Agda.TypeChecking.Unquote
import Agda.TypeChecking.Pretty

import Agda.TypeChecking.CheckInternal

-- import Agda.Syntax.Translation.InternalToAbstract
-- import Agda.TypeChecking.Rules.Term

report = reportSDoc "toTrain" 1
noop   = return ()

-- A training function generates training data for each typed sub-term,
-- with access to the local context via the typechecking monad.
type TrainF = (Type, Term) -> TCM ()

-- an example training function that just prints the relevant (local) information
train :: TrainF
train (ty, t) = do
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
  ty <- typeOfConst defName
  go k ty theDef

class Go a where
  go :: TrainF -> Type -> a -> TCM ()
instance Go Defn where
  go k ty = \case
    (Function{..}) -> mapM_ (go k ty) funClauses
    _ -> report "not a function" >> noop
instance Go Clause where
  go k _ (Clause{..})
    | Just ty <- unArg <$> clauseType
    , Just e  <- clauseBody
    = addContext clauseTel $ do
        go k ty e
    | otherwise
    = noop
instance Go Term where
  go k ty t = do
    report $ prettyTCM t <> " : " <> prettyTCM ty
    k (ty, t)
    case (t, unEl ty) of
      (Lam _ absE, Pi dty ty') -> underAbstraction dty absE $ \e -> do
        -- ty <- tcInferType e
        -- k (ty, e)
        -- e' <- reify e
        -- report $ prettyTCM e <> " ~> " <> prettyTCM e'
        -- ty <- inferExpr e'
        -- report $ "ty: " <> prettyTCM ty
        -- ty <- infer e
        -- k (ty, e)
        go k (unAbs ty') e
      (Var i es, _) -> noop
        -- go k es
      (Def n es, _) -> noop -- go k es
      (Con c ci es, _) -> noop -- go k es
      _ -> noop
-- instance Go a => Go [a] where
--   go k = mapM_ (go k)
instance Go a => Go (Elim' a) where
  go k ty = \case
    Apply a -> go k ty a
    Proj _ _ -> noop
    IApply x y z -> mapM_ (go k ty) [x, y, z]
instance Go a => Go (Arg a) where
  go k ty = go k ty . unArg

-- ** Gathering names from terms
class HasNames a where
  names :: a -> [QName]

  collectNames :: a -> S.Set QName
  collectNames = S.fromList . names

instance HasNames a => HasNames [a] where
  names = concatMap names
instance HasNames a => HasNames (Elim' a) where
  names = \case
    Apply a -> names a
    Proj _ qn -> [qn]
    IApply x y z -> concatMap names [x, y, z]
instance HasNames a => HasNames (Arg a) where
  names = names . unArg
instance HasNames Term where
  names = \case
    (Lam _ absE) -> names (unAbs absE)
    (Var i es) -> {-unQual ('#':show i) :-} names es
    (Def n es) -> n : names es
    (Con c _ es) -> conName c : names es
    (Pi _ _) -> []
    _ -> []

{-
class ToTrain a where
  toTrain :: a -> TCM ()

  -- default toTrain :: PrettyTCM a => a -> TCM ()
  -- toTrain = report . prettyTCM

instance ToTrain Definition where
  toTrain def@(Defn {..}) = do
    -- let cls = listify (const True) theDef
    -- forM_ cls $ toTrain @Clause
    case theDef of
      (Function{..}) -> do
        report $ "Definition: " <> prettyTCM defName
        mapM_ toTrain funClauses
      _ -> return ()

instance ToTrain Clause where
  toTrain (Clause {..}) = toTrain (clauseTel, unArg (fromJust clauseType), fromJust clauseBody)

instance ToTrain (Ctx, Type, Term) where
  toTrain (ctx, ty, t) = do
      -- report "******************************************************************"
      -- printScope "unbound" 1 ""
      -- report "******************************************************************"
      report "{"
      report $ "ctx: "  <> prettyTCM ctx
      report $ "type: " <> prettyTCM ty
      report $ "term: " <> prettyTCM t
      -- let ns = listify (const True) t :: [Name]
      -- report $ "names used: " <> prettyTCM ns
      report "}"

type RCtx = [Arg R.Type]

ctxToAbstract :: RCtx -> Ctx
ctxToAbstract = undefined
-}
