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
import Output ( testJSON )

main :: IO ()
main = do
  as <- getArgs
  -- let extraFlags = []
  let extraFlags = ["--no-projection-like"]
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
    -- sc <- getCurrentScope
    -- report $ prettyTCM (prettyShow $ scopeNameSpaces sc)
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
reportScope scope = do
  sc <- getScope
  debugPrint $ "locals: " <> prettyTCM (prettyShow $ sc ^. scopeLocals)
  reportNamespace $ allThingsInScope scope

reportNamespace :: NameSpace -> TCM ()
reportNamespace (NameSpace names mods ns) = do
  debugPrint $ "ns: " <> prettyTCM (prettyShow ns)
  forM_ (M.toList names) $ \(cn, n) -> do
    let hn = head n
    ty <- typeOfConst (anameName hn)
    -- TODO: clean up qualifiers of the form: CurrentModule.(_.)*
    report $ prettyTCM (prettyShow hn) <> " : " <> prettyTCM (prettyShow ty)
    report $ "  pp: " <> prettyTCM ty
  mapM_ (mapM_ reportModule) (M.elems mods)
