module Main where

import Control.Monad ( mapM_ )

import Agda.Main ( runAgda )
import Agda.Compiler.Backend

import ToTrain ( toTrain )

main :: IO ()
main = runAgda [Backend back]
  where
    back :: Backend' () () () () Definition
    back = Backend'
      { backendName           = "agda2train"
      , options               = ()
      , commandLineFlags      = []
      , isEnabled             = \ _ -> True
      , preCompile            = return
      , postCompile           = \ _ _ _ -> return ()
      , preModule             = \ _ _ _ _ -> return $ Recompile ()
      , compileDef            = \ _ _ _ def -> return def
      , postModule            = \ _ _ _ _ -> mapM_ toTrain
      , backendVersion        = Nothing
      , scopeCheckingSuffices = False
      , mayEraseType          = \ _ -> return True
      }

