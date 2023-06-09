module Main where

import GHC.Generics

import System.Environment ( getArgs, withArgs )
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.FilePath ( (</>) )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..) )

import qualified Data.Set as S
import qualified Data.Aeson as JSON

import Control.Monad
import Control.Monad.IO.Class ( liftIO )
import Control.DeepSeq ( NFData )

import Agda.Main ( runAgda )
import Agda.Compiler.Backend hiding (Reduced)

import Agda.Utils.Pretty
import Agda.Utils.Maybe
import Agda.Utils.Monad

import Agda.Syntax.Scope.Base
import Agda.Syntax.Scope.Monad
import Agda.Syntax.TopLevelModuleName

import Agda.TypeChecking.Reduce

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
mkBackend name trainF = Backend'
  { backendName           = name
  , backendVersion        = Nothing
  , options               = defaultOptions
  , commandLineFlags      =
      [ Option ['r'] ["recurse"] (NoArg recOpt)
        "Recurse into imports/dependencies."
      , Option ['x'] ["no-json"] (NoArg noJsonOpt)
        "Skip generation of JSON files. (just debug print)"
      , Option [] ["ignore-existing-json"] (NoArg ignoreExistingJsonOpt)
        "Ignore existing JSON files. (i.e. always overwrite)"
      , Option ['t'] ["include-definitions"] (NoArg includeDefinitionsOpt)
        "Also include definitions of things in scope"
      , Option ['o'] ["out-dir"] (ReqArg outDirOpt "DIR")
        "Generate data at DIR. (default: project root)"
      ]
  , isEnabled             = \ _ -> True
  , preCompile            = return
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = \ opts isMain md _ ->
      let
        processScopeEntry :: QName -> TCM (Maybe ScopeEntry)
        processScopeEntry qn = do
          -- TODO: clean up qualifiers of the form: CurrentModule.(_.)*
          caseMaybeM (tryMaybe $ typeOfConst qn) (pure Nothing) $ \ty -> do
            pty <- ppm ty
            rty <- mkReduced ty
            def <- getConstInfo qn
            -- pdef <- ppm def
            tdef <- toTerm def
            pdef <- ppm tdef
            report 20 $ ppm (pp qn) <> " : " <> ppm (pp ty)
            report 20 $ ppm (pp qn) <> " = " <> ppm tdef
            report 30 $ "      *pretty: " <> ppm ty
            reportReduced rty
            return $ Just
               $ pp qn
              :~ (render pty :> fmap convert rty)
              := boolToMaybe (includeDefinitions opts)
                             (render pdef :> convert tdef)
      in
        ifM (skip opts isMain md) (return $ Skip ()) $ do
          report 10 $ "************************ "
                   <> ppm md <> " (" <> ppm (show isMain)
                   <> ") ***********************************"
          -- printScope "" 1 ""
          setScope . iInsideScope =<< curIF
          NameSpace _ _ ns <- allThingsInScope <$> getCurrentScope
          scopeEntries <- mapMaybeM processScopeEntry (S.toList ns)
          report 10 "******************************************************************"
          return $ Recompile scopeEntries
  , compileDef            = \ _ _ _ def -> runC $ forEachHole trainF def
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
            || (not noJson && jsonExists && not ignoreExistingJson)

-- ** command-line flags

data Options = Options
  { recurse, noJson, ignoreExistingJson, includeDefinitions :: Bool
  , outDir  :: Maybe FilePath
  } deriving (Generic, NFData)

defaultOptions = Options
  { recurse            = False
  , noJson             = False
  , ignoreExistingJson = False
  , includeDefinitions = True
  , outDir             = Nothing
  }

recOpt, noJsonOpt, ignoreExistingJsonOpt, includeDefinitionsOpt
  :: Monad m => Options -> m Options
recOpt                opts = return $ opts { recurse            = True }
noJsonOpt             opts = return $ opts { noJson             = True }
ignoreExistingJsonOpt opts = return $ opts { ignoreExistingJson = True }
includeDefinitionsOpt opts = return $ opts { includeDefinitions = True }

outDirOpt :: Monad m => FilePath -> Options -> m Options
outDirOpt fp opts = return $ opts { outDir = Just fp }

getOutDir :: Options -> FilePath
getOutDir opts = case outDir opts of
  Just fp -> fp
  Nothing -> "."

getOutFn :: Options -> String -> FilePath
getOutFn opts mn = getOutDir opts </> mn <> ".json"
