module Main where

import GHC.Generics

import System.Environment ( getArgs, withArgs )
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.FilePath ( dropFileName, (</>) )
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

import Agda.Syntax.Position
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
      [ Option ['r'] ["recurse"] (NoArg recOpt)
        "Recurse into imports/dependencies."
      , Option ['o'] ["out-dir"] (ReqArg outDirOpt "DIR")
        "Generate data at DIR. (default: project root)"
      , Option ['x'] ["no-json"] (NoArg noJsonOpt)
        "Skip generation of JSON files. (just debug print)"
      ]
  , isEnabled             = \ _ -> True
  , preCompile            = return
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = \ opts isMain md mfp ->
      ifM (skip opts isMain md) (return $ Skip ()) $ do
        reportTCM 10 $ "************************ "
                    <> ppm md <> " (" <> ppm (show isMain)
                    <> ") ***********************************"
        -- printScope "" 1 ""
        setScope . iInsideScope =<< curIF
        NameSpace names mods ns <- allThingsInScope <$> getCurrentScope
        scopeEntries <- mapMaybeM processScopeEntry (S.toList ns)
        reportTCM 10 "******************************************************************"
        return $ Recompile scopeEntries
  , compileDef            = \ _ _ _ def -> runC $ forEachHole train def
  , postModule            = \ opts scopeEntries isMain md samples -> let mn = pp md in
    unlessM (skip opts isMain md) $ do
      whenJust (outDir opts) (liftIO . createDirectoryIfMissing True)
      unless (noJson opts) $
        liftIO $ JSON.encodeFile (getOutFn opts mn) $ TrainData
        { scope   = mn :~ scopeEntries
        , samples = concat samples
        }
  , scopeCheckingSuffices = False
  , mayEraseType          = \ _ -> return True
  }
  where
    skip :: Options -> IsMain -> TopLevelModuleName -> TCM Bool
    skip opts@Options{..} isMain md = do
      jsonExists <- liftIO $ doesFileExist (getOutFn opts $ pp md)
      return $ (not recurse && (isMain == NotMain))
            || (not noJson && jsonExists)

    processScopeEntry :: QName -> TCM (Maybe ScopeEntry)
    processScopeEntry qn = do
      -- TODO: clean up qualifiers of the form: CurrentModule.(_.)*
      caseMaybeM (tryMaybe $ typeOfConst qn) (pure Nothing) $ \ty -> do
        pty <- ppm ty
        reportTCM 20 $ ppm (pp qn) <> " : " <> ppm (pp ty)
        -- reportTCM 20 $ "(range) " <> ppm (getRange $ nameBindingSite $ qnameName qn)
        -- reportTCM 20 $ "(rangeCur) " <> (ppm =<< asksTC envHighlightingRange)
        reportTCM 30 $ "  pp: " <> ppm ty
        return $ Just (pp qn :~ render pty :> convert ty)

-- ** command-line flags

data Options = Options
  { recurse :: Bool
  , outDir  :: Maybe FilePath
  , noJson  :: Bool
  } deriving (Generic, NFData)

defaultOptions = Options
  { recurse = False
  , outDir = Nothing
  , noJson = False
  }

recOpt, noJsonOpt :: Monad m => Options -> m Options
recOpt    opts = return $ opts { recurse = True }
noJsonOpt opts = return $ opts { noJson  = True }

outDirOpt :: Monad m => FilePath -> Options -> m Options
outDirOpt fp opts = return $ opts { outDir = Just fp }

getOutDir :: Options -> FilePath
getOutDir opts = case outDir opts of
  Just fp -> fp
  Nothing -> "."

getOutFn :: Options -> String -> FilePath
getOutFn opts mn = getOutDir opts </> mn <> ".json"
