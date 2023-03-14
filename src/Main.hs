module Main where

import Control.Monad ( mapM_ )

import Agda.Main ( runAgda )
import Agda.Compiler.Backend

import Agda.Syntax.Scope.Monad

import ToTrain

main :: IO ()
main = runAgda [mkBackend "agda2train" train]

mkBackend :: String -> TrainF -> Backend
mkBackend name train = Backend $ Backend'
  { backendName           = name
  , options               = ()
  , commandLineFlags      = []
  , isEnabled             = \ _ -> True
  , preCompile            = return
  , postCompile           = \ _ _ _ -> return ()
  , preModule             = \ _ _ _ _ -> return $ Recompile ()
  , compileDef            = \ _ _ _ def -> return def
  , postModule            = \ _ _ _ _ defs -> do
    sc <- getCurrentScope
    report "************************ SCOPE ***********************************"
    printScope "unbound" 1 ""
    report "******************************************************************"
    mapM_ (forEachHole train) defs
  , backendVersion        = Nothing
  , scopeCheckingSuffices = False
  , mayEraseType          = \ _ -> return True
  }

