module Main where

import Control.Monad ( mapM_ )

import Agda.Main ( runAgda )
import Agda.Compiler.Backend

import Agda.Syntax.Scope.Base
import Agda.Syntax.Scope.Monad
import Agda.TypeChecking.Pretty
-- import Agda.Utils.Pretty

import ToTrain

-- github :: IO ()
-- github =

main :: IO ()
main = runAgda [mkBackend "agda2train" train]

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
  , postModule            = \ () () isMain md defs -> do
    sc <- getCurrentScope
    -- md <- getCurrentModule
    report $ "************************ " <> prettyTCM md <> " (" <> prettyTCM (show isMain) <> ") ***********************************"
    -- printScope "unbound" 1 ""
    -- report $ return $ text $ prettyShow $ everythingInScope sc
    -- report "******************************************************************"
    mapM_ (forEachHole train) defs
  , scopeCheckingSuffices = False
  , mayEraseType          = \ _ -> return True
  }

