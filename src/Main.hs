{-# LANGUAGE TypeApplications #-}
-- | Main driver for the @agda2train@ executable.
module Main where

import GHC.Generics

import System.Environment ( getArgs, withArgs )
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..) )

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BL

import Control.Monad
import Control.Monad.Reader ( runReaderT )
import Control.Monad.IO.Class ( liftIO )

import Agda.Main ( runAgda )
import Agda.Compiler.Backend hiding (Reduced)
import Agda.Compiler.Common ( curDefs )

import Agda.Utils.Pretty
import Agda.Utils.Maybe
import Agda.Utils.Monad

import Agda.Syntax.Scope.Base
import Agda.Syntax.Scope.Monad
import Agda.Syntax.TopLevelModuleName

import Agda.TypeChecking.Reduce
import qualified Agda.TypeChecking.Pretty as P

import Agda.Compiler.Common ( curIF )

import Options
import ToTrain
import JSON
import InternalToJSON

-- | The main entrypoint for the @agda2train@ executable.
main :: IO ()
main = do
  as <- getArgs
  let extraFlags = [] -- ["--no-projection-like"]
  withArgs (extraFlags ++ as) $
    runAgda [Backend $ mkBackend train]

-- | Make an Agda backend from a given training function (c.f. 'ToTrain.TrainF').
--
-- * Command-line flags (run `agda2train --help` to display this)
--
-- agda2train backend options
--   -r      --recurse               Recurse into imports/dependencies.
--   -x      --no-json               Skip generation of JSON files. (just debug print)
--           --ignore-existing-json  Ignore existing JSON files. (i.e. always overwrite)
--           --print-json            Print JSON output. (for debugging)
--           --no-terms              Do not include definitions of things in scope
--           --no-privates           Do not include private definitions
--           --no-dep-levels         Do not include dependency levels
--   -o DIR  --out-dir=DIR           Generate data at DIR. (default: project root)
mkBackend :: TrainF ->
  Backend' Options Options ([ScopeEntry], [ScopeEntry]) () (String, [Sample])
mkBackend trainF = Backend'
  { backendName      = "agda2train"
  , backendVersion   = Nothing
  , options          = defaultOptions
  , commandLineFlags =
      [ Option ['r'] ["recurse"] (NoArg recOpt)
        "Recurse into imports/dependencies."
      , Option ['x'] ["no-json"] (NoArg noJsonOpt)
        "Skip generation of JSON files. (just debug print)"
      , Option [] ["ignore-existing-json"] (NoArg ignoreJsonOpt)
        "Ignore existing JSON files. (i.e. always overwrite)"
      , Option [] ["print-json"] (NoArg printJsonOpt)
        "Print JSON output. (for debugging)"
      , Option [] ["no-terms"] (NoArg includeDefsOpt)
        "Do not include definitions of things in scope"
      , Option [] ["no-privates"] (NoArg includePrivsOpt)
        "Do not include private definitions"
      , Option [] ["no-dep-levels"] (NoArg includePrivsOpt)
        "Do not include private definitions"
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
        processScopeEntry qn =
          caseMaybeM (tryMaybe $ do
            def <- getConstInfo qn
            case theDef def of
              GeneralizableVar -> fail "not handling `variable` definitions"
              DataOrRecSig _   -> fail "not handling `DataOrRecSig` definitions"
              _ -> typeOfConst qn) (pure Nothing) $ \ty -> do
            rty  <- mkReduced ty
            pty  <- ppm ty
            rty' <- runReaderT (traverse convert rty) opts

            def  <- getConstInfo qn
            pdef <- ppm def
            def' <- runReaderT (convert def) opts

            report 20 $ ppm (pp qn) <> " : " <> ppm ty
            report 30 $ "      *pretty: " <> P.text (pp ty)
            report 20 $ ppm (pp qn) <> " = " <> ppm def
            report 30 $ "      *pretty: " <> P.text (pp def)
            reportReduced rty
            report 20 ""

            return $ Just $ ppName qn :~ ScopeEntry
              { _type      = prender pty :> rty'
              , definition = boolToMaybe (includeDefs opts)
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
          -- ** public
          NameSpace _ _ ns <- allThingsInScope <$> getCurrentScope
          scopeEntries <- mapMaybeM processScopeEntry (S.toList ns)
          -- ** private
          privs <- HM.filterWithKey (\k _ -> k `S.notMember` ns) <$> liftTCM curDefs
          privScopeEntries <- mapMaybeM processScopeEntry (HM.keys privs)
          report 10 "******************************************************************"
          return $ Recompile (scopeEntries, privScopeEntries)
  , compileDef = \ opts _ _ def ->
      (ppName (defName def) ,) <$> runC opts (forEachHole trainF def)
  , postModule = \ opts (scopeEntries, privScopeEntries) isMain md samples ->
    let mn = pp md in
    unlessM (skip opts isMain md) $ do
      whenJust (outDir opts) (liftIO . createDirectoryIfMissing True)
      unless (noJson opts) $ liftIO $ do
        let
          isGlobal = \(n :~ _) -> isNothing $ lookup n samples
          (globals, locals') = L.partition isGlobal scopeEntries
          locals = flip map locals' $ \(n :~ l) ->
              n :~ l { holes = Just $ fromJust $ lookup n samples }
          plocals = flip map privScopeEntries $ \(n :~ l) ->
              n :~ l { holes = Just $ fromJust $ lookup n samples }
          fileData = mn :~ TrainData
            { scopeGlobal  = globals
            , scopeLocal   = locals
            , scopePrivate = boolToMaybe (includePrivs opts) plocals
            }

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
            || (not noJson && jsonExists && not ignoreJson)
