-- {-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module ToTrain where

import Data.Maybe ( fromJust )
import Data.Generics ( listify )
import Control.Monad ( forM_ )

-- import Agda.Compiler.Common

import Agda.Syntax.Common
import Agda.Syntax.Internal

import Agda.TypeChecking.Monad
import Agda.TypeChecking.Pretty

-- import qualified Agda.Utils.Pretty as P

import qualified Agda.Syntax.Reflected as R

report = reportSDoc "toTrain" 1

class ToTrain a where
  toTrain :: a -> TCM ()

  -- default toTrain :: PrettyTCM a => a -> TCM ()
  -- toTrain = report . prettyTCM

instance ToTrain Definition where
  toTrain def@(Defn {..}) = do
    -- let cls = listify (const True) theDef
    -- forM_ cls $ toTrain @Clause
    case theDef of
      (Function{..}) -> do
        report $ "Definition: " <> prettyTCM defName
        mapM_ toTrain funClauses
      _ -> return ()

instance ToTrain Clause where
  toTrain (Clause {..}) = toTrain (clauseTel, unArg (fromJust clauseType), fromJust clauseBody)

type Ctx = Telescope

instance ToTrain (Ctx, Type, Term) where
  toTrain (ctx, ty, t) = do
      report "******************************************************************"
      printScope "unbound" 1 ""
      report "******************************************************************"
      report "{"
      report $ "ctx: "  <> prettyTCM ctx
      report $ "type: " <> prettyTCM ty
      report $ "term: " <> prettyTCM t
      -- let ns = listify (const True) t :: [Name]
      -- report $ "names used: " <> prettyTCM ns
      report "}"

type RCtx = [Arg R.Type]

ctxToAbstract :: RCtx -> Ctx
ctxToAbstract = undefined



