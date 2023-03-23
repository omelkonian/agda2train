module ToTrain
  ( TrainF
  , train
  , forEachHole
  , runC
  , reportTCM
  , ppm
  ) where

import Data.List ( isPrefixOf, isInfixOf )
import qualified Data.Set as S

import Control.Monad ( forM_, void, when, unless )
import Control.Monad.Writer ( WriterT(runWriterT) )
import Control.Monad.Error.Class ( catchError )

import Text.PrettyPrint ( render )

import Agda.Syntax.Common ( unArg )
import Agda.Syntax.Abstract.Name ( QName(..) )
import Agda.Syntax.Internal
import Agda.Syntax.Internal.Generic ( TermLike, foldTerm )
import Agda.Syntax.Scope.Base ( nsInScope, allThingsInScope )
import Agda.Syntax.Scope.Monad ( getCurrentScope )
import Agda.Utils.Monad ( whenM, tell1 )
import Agda.TypeChecking.Monad
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.CheckInternal ( Action(..), defaultAction, checkInternal' )

import AgdaInternals
import Output

ppm :: PrettyTCM a => a -> TCM Doc
ppm = prettyTCM
reportTCM = reportSDoc "toTrain"
report k = liftTCM . reportTCM k

type C = WriterT [Sample] TCM

runC :: C () -> TCM [Sample]
runC = (snd <$>) . runWriterT

noop :: C ()
noop = return ()

silently :: C a -> C ()
silently k = void k `catchError` \ _ -> noop

-- A training function generates training data for each typed sub-term,
-- with access to the local context via the typechecking monad.
type TrainF = Type -> Term -> C ()

-- an example training function that just prints the relevant (local) information
train :: TrainF
train ty t = do
  ctx <- liftTCM getContextTelescope
  let ns = names t
  allNs <- nsInScope . allThingsInScope <$> liftTCM getCurrentScope
  unless (null ns) $
    -- TODO: we drop samples that use private definitions for now, but it would
    -- be more informative to peek into a module's private parts...
    when (S.fromList ns `S.isSubsetOf` allNs) $ do
      ctx <- getContextTelescope
      pctx <- liftTCM $ ppm ctx
      pty  <- liftTCM $ ppm ty
      pt   <- liftTCM $ ppm t
      -- TODO: figure out a way to run Agsy.Auto here and provide training data
      -- on successful invocations only
      tell1 $ Sample
        { ctx  = render pctx :> convert ctx
        , goal = render pty  :> convert ty
        , term = render pt   :> convert t
        , namesUsed = map pp ns
        }
      report 20 "{"
      report 20 $ " ctx: " <> ppm (pp ctx)
      report 30 $ "   pp:" <> ppm ctx
      report 20 $ " goal: " <> ppm (pp ty)
      report 30 $ "   pp:" <> ppm ty
      report 20 $ " term: " <> ppm (pp t)
      report 30 $ "   names: " <> ppm ns
      report 30 $ "   pp: " <> ppm t
      report 20 "}"

-- Run the training function on each subterm of a definition.
forEachHole :: TrainF -> Definition -> C ()
forEachHole k def@Defn{..} = do
  report 10 $ "------ definition: " <> ppm defName <> " -------"
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
       || tooSlow (pp defName)
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

    cubicalRelated, tooSlow :: String -> Bool
    cubicalRelated = ("Agda.Primitive.Cubical.I" `isInfixOf`)
    tooSlow        = ("Data.Rational.Properties" `isPrefixOf`)

    go :: Type -> Term -> C ()
    go ty t = whenM (not <$> ignore ty)
            $ silently (checkInternal' act t CmpLeq ty)

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
