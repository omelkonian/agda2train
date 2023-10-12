{-# LANGUAGE TypeApplications #-}
module Main where

import GHC.Generics

import System.Environment ( getArgs, withArgs )
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.FilePath ( (</>) )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..) )

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BL
import Data.Aeson ( ToJSON )
import Data.Aeson.Encode.Pretty
  ( encodePretty', Config(..), defConfig, Indent(..), keyOrder )

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
import qualified Agda.TypeChecking.Pretty as P

import Agda.Compiler.Common ( curIF )

import ToTrain
import Output

main :: IO ()
main = do
  as <- getArgs
  let extraFlags = []
  -- let extraFlags = ["--no-projection-like"]
  withArgs (extraFlags ++ as) $
    runAgda [Backend $ mkBackend train]

mkBackend :: TrainF -> Backend' Options Options [ScopeEntry] () (String, [Sample])
mkBackend trainF = Backend'
  { backendName      = "agda2train"
  , backendVersion   = Nothing
  , options          = defaultOptions
  , commandLineFlags =
      [ Option ['r'] ["recurse"] (NoArg recOpt)
        "Recurse into imports/dependencies."
      , Option ['x'] ["no-json"] (NoArg noJsonOpt)
        "Skip generation of JSON files. (just debug print)"
      , Option [] ["ignore-existing-json"] (NoArg ignoreExistingJsonOpt)
        "Ignore existing JSON files. (i.e. always overwrite)"
      , Option [] ["print-json"] (NoArg printJsonOpt)
        "Print JSON output. (for debugging)"
      , Option [] ["no-terms"] (NoArg includeDefinitionsOpt)
        "Do not include definitions of things in scope"
      , Option ['o'] ["out-dir"] (ReqArg outDirOpt "DIR")
        "Generate data at DIR. (default: project root)"
      ]
  , isEnabled             = \ _ -> True
  , scopeCheckingSuffices = False
  , mayEraseType          = \ _ -> return True
  , preCompile  = return
  , postCompile = \ _ _ _ -> return ()
  , preModule   = \ opts isMain md _ ->
      let
        processScopeEntry :: QName -> TCM (Maybe ScopeEntry)
        processScopeEntry qn = do
          -- TODO: clean up qualifiers of the form: CurrentModule.(_.)*
          caseMaybeM (tryMaybe $ typeOfConst qn) (pure Nothing) $ \ty -> do
            rty  <- mkReduced ty
            pty  <- ppm ty
            rty' <- traverse convert rty

            def  <- getConstInfo qn
            pdef <- ppm def
            def' <- convert def

            report 20 $ ppm (pp qn) <> " : " <> ppm ty
            report 30 $ "      *pretty: " <> P.text (pp ty)
            report 20 $ ppm (pp qn) <> " = " <> ppm def
            report 30 $ "      *pretty: " <> P.text (pp def)
            reportReduced rty
            report 20 ""

            return $ Just $ ppName qn :~ ScopeEntry
              { _type      = prender pty :> rty'
              , definition = boolToMaybe (includeDefinitions opts)
                           $ prender pdef :> def'
              , holes = Nothing
              }
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
  , compileDef = \ _ _ _ def -> (ppName (defName def) ,) <$> runC (forEachHole trainF def)
  , postModule = \ opts scopeEntries isMain md samples -> let mn = pp md in
    unlessM (skip opts isMain md) $ do
      whenJust (outDir opts) (liftIO . createDirectoryIfMissing True)
      unless (noJson opts) $ liftIO $ do
        let
          isGlobal = \(n :~ _) -> isNothing $ lookup n samples
          (globals, locals') = L.partition isGlobal scopeEntries
          locals = flip map locals' $ \(n :~ l) ->
              n :~ l { holes = Just $ fromJust $ lookup n samples }

          fileData = mn :~ TrainData {scopeGlobal = globals, scopeLocal = locals}

        encodeFile (getOutFn opts mn) fileData
        when (printJson opts) $ do
          putStrLn "************************ JSON ************************"
          BL.putStr (encode fileData)
          putStrLn "******************************************************"
  }
  where
    skip :: Options -> IsMain -> TopLevelModuleName -> TCM Bool
    skip opts@Options{..} isMain md = do
      jsonExists <- liftIO $ doesFileExist (getOutFn opts $ pp md)
      return $ (not recurse && (isMain == NotMain))
            || (not noJson && jsonExists && not ignoreExistingJson)

-- ** command-line flags

data Options = Options
  { recurse, noJson, ignoreExistingJson, printJson, includeDefinitions :: Bool
  , outDir  :: Maybe FilePath
  } deriving (Generic, NFData)

defaultOptions = Options
  { recurse            = False
  , noJson             = False
  , ignoreExistingJson = False
  , printJson          = False
  , includeDefinitions = True
  , outDir             = Nothing
  }

recOpt, noJsonOpt, ignoreExistingJsonOpt, printJsonOpt, includeDefinitionsOpt
  :: Monad m => Options -> m Options
recOpt                opts = return $ opts { recurse            = True }
noJsonOpt             opts = return $ opts { noJson             = True }
printJsonOpt          opts = return $ opts { printJson          = True }
ignoreExistingJsonOpt opts = return $ opts { ignoreExistingJson = True }
includeDefinitionsOpt opts = return $ opts { includeDefinitions = False }

outDirOpt :: Monad m => FilePath -> Options -> m Options
outDirOpt fp opts = return $ opts { outDir = Just fp }

getOutDir :: Options -> FilePath
getOutDir opts = case outDir opts of
  Just fp -> fp
  Nothing -> "."

getOutFn :: Options -> String -> FilePath
getOutFn opts mn = getOutDir opts </> mn <> ".json"

-- ** JSON encoding

encode :: ToJSON a => a -> BL.ByteString
encode = encodePretty' $ defConfig
  { confIndent = Spaces 2
  , confCompare = keyOrder
      [ "pretty"
      , "tag"
      , "name"
      , "original", "simplified", "reduced", "normalised"
      , "telescope", "patterns"
      , "domain", "codomain"
      , "abstraction", "body"
      , "sort", "level", "literal"
      , "head", "arguments"
      , "variants", "reference", "variant"
      , "index"
      , "scopeGlobal", "scopeLocal"
      , "type", "definition", "holes"
      , "ctx", "goal", "term", "premises"
      ]
  }

encodeFile :: ToJSON a => FilePath -> a -> IO ()
encodeFile = \fn -> BL.writeFile fn . encode
