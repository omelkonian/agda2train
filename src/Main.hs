module Main where

import GHC.Generics

import System.Environment ( getArgs, withArgs )
import System.Directory ( setCurrentDirectory, createDirectoryIfMissing )
import System.FilePath ( dropFileName )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..) )

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Aeson as JSON

import Control.Monad
import Control.Monad.IO.Class ( liftIO )
import Control.DeepSeq ( NFData )

import Agda.Main ( runAgda )
import Agda.Compiler.Backend

import Agda.Utils.Pretty
import Agda.Utils.Lens
import Agda.Utils.BiMap
import Agda.Utils.Maybe
import Agda.Utils.Monad

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

mkBackend :: String -> TrainF -> Backend' Options Options [ScopeEntry] () [Sample]
mkBackend name train = Backend'
  { backendName           = name
  , backendVersion        = Nothing
  , options               = defaultOptions
  , commandLineFlags      =
      [ Option ['r'] [] (NoArg recOpt) "Recurse into imports/dependencies."
      ]
  , isEnabled             = \ _ -> True
  , preCompile            = return
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = \ opts isMain md mfp ->
      if skip opts isMain then return $ Skip () else do
        reportTCM 10 $ "************************ "
                    <> ppm md <> " (" <> ppm (show isMain)
                    <> ") ***********************************"
        -- printScope "" 1 ""
        setScope . iInsideScope =<< curIF
        NameSpace names mods ns <- allThingsInScope <$> getCurrentScope
        scopeEntries <- mapMaybeM processScopeEntry (S.toList ns)
        reportTCM 10 "******************************************************************"
        whenJust mfp (liftIO . setCurrentDirectory . dropFileName)
        return $ Recompile scopeEntries
  , compileDef            = \ _ _ _ def -> runC $ forEachHole train def
  , postModule            = \ opts scopeEntries isMain md samples ->
    unless (skip opts isMain) $ do
      mn <- render <$> ppm md
      liftIO $ JSON.encodeFile (mn <> ".json") $ TrainData
        { scope   = mn :~ scopeEntries
        , samples = concat samples
        }
  , scopeCheckingSuffices = False
  , mayEraseType          = \ _ -> return True
  }
  where
    skip :: Options -> IsMain -> Bool
    skip opts isMain = not (recurse opts) && (isMain == NotMain)

    processScopeEntry :: QName -> TCM (Maybe ScopeEntry)
    processScopeEntry qn = do
      -- TODO: clean up qualifiers of the form: CurrentModule.(_.)*
      caseMaybeM (tryMaybe $ typeOfConst qn) (pure Nothing) $ \ty -> do
        pty <- ppm ty
        reportTCM 20 $ ppm (pp qn) <> " : " <> ppm (pp ty)
        reportTCM 30 $ "  pp: " <> ppm ty
        return $ Just (pp qn :~ render pty :> convert ty)

data Options = Options {recurse :: Bool} deriving (Generic, NFData)

defaultOptions = Options { recurse = False }

recOpt :: Monad m => Options -> m Options
recOpt opts = return $ opts { recurse = True }
