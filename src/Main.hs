module Main where

import GHC.Generics

import System.Environment ( getArgs, withArgs )
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.FilePath ( (</>) )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..) )

import qualified Data.Set as S
import Data.Aeson ( ToJSON )
import Data.Aeson.Encode.Pretty
  ( encodePretty', Config(..), defConfig, Indent(..), keyOrder )
import qualified Data.ByteString.Lazy as L

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

mkBackend :: String -> TrainF -> Backend' Options Options [ScopeEntry] () (String, [Sample])
mkBackend name trainF = Backend'
  { backendName      = name
  , backendVersion   = Nothing
  , options          = defaultOptions
  , commandLineFlags =
      [ Option ['r'] ["recurse"] (NoArg recOpt)
        "Recurse into imports/dependencies."
      , Option ['x'] ["no-json"] (NoArg noJsonOpt)
        "Skip generation of JSON files. (just debug print)"
      , Option [] ["ignore-existing-json"] (NoArg ignoreExistingJsonOpt)
        "Ignore existing JSON files. (i.e. always overwrite)"
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
            return $ Just $ pp qn :~ ScopeEntry
              { _type      = render pty :> fmap convert rty
              , definition = boolToMaybe (includeDefinitions opts)
                                         (render pdef :> convert tdef)
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
  , compileDef = \ _ _ _ def -> (pp (defName def) ,) <$> runC (forEachHole trainF def)
  , postModule = \ opts scopeEntries isMain md samples -> let mn = pp md in
    unlessM (skip opts isMain md) $ do
      whenJust (outDir opts) (liftIO . createDirectoryIfMissing True)
      unless (noJson opts) $ liftIO $ do
        putStrLn "************************ JSON ************************"
        let
          -- divideEntries :: [ScopeEntry] -> [Samples] -> ([ScopeEntry], [ScopeEntry])
          -- divideEntries [] _ = []
          -- divideEntries (x:xs) () =
          --   let
          --     (globals, locals) = divideEntries xs
          --   in
          --     case
          --     (x:globals, locals)
          --     (globals, x:locals)

          -- globals , locals = divideEntries scopeEntries samples

          (globals, locals') = span (\(n :~ _) -> isNothing $ lookup n samples) scopeEntries
          locals = flip map locals' $ \(n :~ l) ->
            n :~ l {holes = Just $ fromJust $ lookup n samples }


          fileData = mn :~ TrainData {scopeGlobal = globals, scopeLocal = locals}

        encodeFile (getOutFn opts mn) fileData
        L.putStr (encode fileData)
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

encode :: ToJSON a => a -> L.ByteString
encode = encodePretty' $ defConfig
  { confIndent = Spaces 2
  , confCompare = keyOrder
      [ "pretty"
      , "tag"
      , "name"
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
  where
    -- named   = keyOrder . (["name", "item"] <>)
    -- pretty  = keyOrder . (["pretty", "thing"] <>)
    -- tagged  = keyOrder . ("tag" :)
    -- reduced = ["original", "simplified", "reduced", "normalised"]

encodeFile :: ToJSON a => FilePath -> a -> IO ()
encodeFile = \fn -> L.writeFile fn . encode
