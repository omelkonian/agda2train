module Main where

import System.Environment ( getArgs, withArgs )
import Control.Monad
import qualified Data.Map as M

import Agda.Main ( runAgda )
import Agda.Compiler.Backend

import Agda.Utils.Pretty
import Agda.Utils.Lens
import Agda.Utils.BiMap

import Agda.Syntax.Scope.Base
import Agda.Syntax.Scope.Monad
import Agda.Syntax.Internal
import Agda.Syntax.TopLevelModuleName

import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Monad.Signature

import ToTrain


main :: IO ()
main = do
  as <- getArgs
  let extraFlags = []
  -- let extraFlags = ["--no-projection-like"]
  withArgs (extraFlags ++ as) $
    runAgda [mkBackend "agda2train" train]

mkBackend :: String -> TrainF -> Backend
mkBackend name train = Backend $ Backend'
  { backendName           = name
  , backendVersion        = Nothing
  , options               = ()
  , commandLineFlags      = []
  , isEnabled             = \ _ -> True
  , preCompile            = return
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = \ _ _ _ _ -> return $ Recompile ()
  , compileDef            = \ _ _ _ def -> return def
  , postModule            = \ () () isMain md defs -> unless (isMain == NotMain) $ do
    report $ "************************ " <> prettyTCM md <> " ("
          <> prettyTCM (show isMain) <> ") ***********************************"
    -- printScope "" 1 ""
    reportScope =<< getCurrentScope
    report "******************************************************************"
    mapM_ (forEachHole train) defs
  , scopeCheckingSuffices = False
  , mayEraseType          = \ _ -> return True
  }

reportModuleName :: ModuleName -> TCM ()
reportModuleName = (reportScope =<<) . getNamedScope

reportModule :: AbstractModule -> TCM ()
reportModule = reportModuleName . amodName

reportScope :: Scope -> TCM ()
reportScope = reportNamespace . allThingsInScope

reportNamespace :: NameSpace -> TCM ()
reportNamespace (NameSpace names mods _) = do
  forM_ (M.toList names) $ \(cn, n) -> do
    ty <- typeOfConst (anameName (head n))
    report $ prettyTCM (prettyShow cn) <> " : " <> prettyTCM ty
  mapM_ (mapM_ reportModule) (M.elems mods)
