module Main where

import System.Environment ( getArgs, withArgs )
import System.Directory ( setCurrentDirectory, createDirectoryIfMissing )
import System.FilePath ( dropFileName )
import Control.Monad
import Control.Monad.IO.Class ( liftIO )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Aeson as JSON

import Agda.Main ( runAgda )
import Agda.Compiler.Backend

import Agda.Utils.Pretty
import Agda.Utils.Lens
import Agda.Utils.BiMap
import Agda.Utils.Maybe

import Agda.Syntax.Scope.Base
import Agda.Syntax.Scope.Monad
import Agda.Syntax.Internal
import Agda.Syntax.TopLevelModuleName

import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Monad.Signature

import Agda.Compiler.Common ( curIF )

import ToTrain
import Output

main :: IO ()
main = do
  as <- getArgs
  let extraFlags = []
  -- let extraFlags = ["--no-projection-like"]
  withArgs (extraFlags ++ as) $
    runAgda [Backend $ mkBackend "agda2train" train]

mkBackend :: String -> TrainF -> Backend' () () [ScopeEntry] () [Sample]
mkBackend name train = Backend'
  { backendName           = name
  , backendVersion        = Nothing
  , options               = ()
  , commandLineFlags      = []
  , isEnabled             = \ _ -> True
  , preCompile            = return
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = \ _ isMain md mfp -> do
      -- if isMain == NotMain then return $ Skip () else $ do
      reportTCM $ "************************ "
                <> ppm md <> " (" <> ppm (show isMain)
                <> ") ***********************************"
      -- printScope "" 1 ""
      setScope . iInsideScope =<< curIF
      NameSpace names mods ns <- allThingsInScope <$> getCurrentScope
      scopeEntries <- forM (S.toList ns) $ \qn -> do
        ty  <- typeOfConst qn
        -- TODO: clean up qualifiers of the form: CurrentModule.(_.)*
        pty <- ppm ty
        reportTCM $ ppm (pp qn) <> " : " <> ppm (pp ty)
        reportTCM $ "  pp: " <> ppm ty
        return (pp qn, (render pty, convert ty))
      reportTCM "******************************************************************"

      whenJust mfp (liftIO . setCurrentDirectory . dropFileName)
      return $ Recompile scopeEntries
  , compileDef            = \ _ _ _ def -> runC $ forEachHole train def
  , postModule            = \ _ scopeEntries isMain md samples -> {-unless (isMain == NotMain) $-} do
    mn <- render <$> ppm md
    liftIO $ JSON.encodeFile (mn <> ".json") $ TrainData
      { scope   = (mn, scopeEntries)
      , samples = concat samples
      }
  , scopeCheckingSuffices = False
  , mayEraseType          = \ _ -> return True
  }
