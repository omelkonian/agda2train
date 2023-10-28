{-# LANGUAGE TemplateHaskell #-}
-- | Configuration options for the `agda2train` executable.
module Options where

import GHC.Generics ( Generic )
import Control.DeepSeq ( NFData )
import System.FilePath ( (</>) )

-- | Available options to configure how @agda2train@ generates training data:
--
-- ['recurse'] whether to recursively generate data from all transitive dependencies
--
-- ['noJson'] mock run without generating any actual JSON files
--
-- ['ignoreJson'] compile everything from scratch
--
-- ['includeDefs'] whether to include definitions in scope, or just their type
--
-- ['includePrivs'] whether to include private definitions as well
--
data Options = Options
  { recurse, noJson, ignoreJson, printJson,
      includeDefs, includePrivs, includeDepLvls :: Bool
  , outDir  :: Maybe FilePath
  } deriving (Generic, NFData)

-- | The default options.
defaultOptions = Options
  { recurse        = False
  , noJson         = False
  , ignoreJson     = False
  , printJson      = False
  , includeDefs    = True
  , includePrivs   = True
  , includeDepLvls = True
  , outDir         = Nothing
  }

recOpt, noJsonOpt, ignoreJsonOpt, printJsonOpt,
  includeDefsOpt, includePrivsOpt, includeDepLvlsOpt
    :: Monad m => Options -> m Options
recOpt            opts = return $ opts { recurse        = True }
noJsonOpt         opts = return $ opts { noJson         = True }
printJsonOpt      opts = return $ opts { printJson      = True }
ignoreJsonOpt     opts = return $ opts { ignoreJson     = True }
includeDefsOpt    opts = return $ opts { includeDefs    = False }
includePrivsOpt   opts = return $ opts { includePrivs   = False }
includeDepLvlsOpt opts = return $ opts { includeDepLvls = False }

outDirOpt :: Monad m => FilePath -> Options -> m Options
outDirOpt fp opts = return $ opts { outDir = Just fp }

getOutDir :: Options -> FilePath
getOutDir opts = case outDir opts of
  Just fp -> fp
  Nothing -> "."

getOutFn :: Options -> String -> FilePath
getOutFn opts mn = getOutDir opts </> mn <> ".json"
