{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleInstances, DerivingVia #-}
module Output (Sample(..), TrainData(..), ScopeEntry, convert, pp, testJSON) where

import Data.Maybe ( fromMaybe )
import Control.Arrow ( second )

import GHC.Generics
import Data.Aeson

import qualified Agda.Syntax.Common as A
import Agda.Syntax.Common ( unArg )
import qualified Agda.Syntax.Internal as A
import Agda.Syntax.Internal ( absName, unAbs, unEl, unDom )
import qualified Agda.Syntax.Internal.Elim as A
import qualified Agda.TypeChecking.Telescope as A

import Agda.Utils.Pretty

pp :: Pretty a => a -> String
pp = prettyShow

-- ** types

type Name = String
type DB   = Int
type Head = Either Name DB
type Named a = (Name, a)
type WithPretty a = (String, a)

data TrainData = TrainData
  { scope   :: Named [ScopeEntry]
  , samples :: [Sample]
  } deriving Generic
    deriving ToJSON via Generically TrainData

type ScopeEntry = Named (WithPretty Type)

data Sample = Sample
  { ctx   :: WithPretty Telescope
  , goal  :: WithPretty Type
  , term  :: WithPretty Term
  , namesUsed :: [Name]
  } deriving Generic
    deriving ToJSON via Generically Sample

type Telescope = [Named Type]

data Term
  = Pi (Named Term) Term -- ^ e.g. `∀ {A : Set}. A → A`
  | Lam (Named Term)     -- ^ e.g. `λ x. x`
  | App Head [Term]      -- ^ e.g. `f x (x + x)` or `@0 (λ x. x)`
  | Lit String | Sort String | Level String -- ^ e.g. Set/42/"sth",0ℓ,...
  deriving (Generic, Show)
  deriving ToJSON   via Generically Term
  deriving FromJSON via Generically Term

type Type = Term

testJSON :: IO ()
testJSON = do
  let ty = Pi ("A", Sort "Set") (Pi ("_", App (Left "A") []) (App (Left "A") []))
      t = Lam ("a", App (Right 0) [])
  encodeFile "type.json" ty >> encodeFile "term.json" t
  Just ty <- decodeFileStrict "type.json" :: IO (Maybe Term)
  putStrLn $ "ty: " <> show ty
  Just t <- decodeFileStrict "term.json" :: IO (Maybe Term)
  putStrLn $ "t: " <> show t

-- ** conversion from Agda's iternal syntax

class From a where
 type To a
 convert, go :: a -> To a
 convert = go

-- instance From A.?? where
--   type To A.?? = TrainData
--   go =

instance From A.Telescope where
  type To A.Telescope = Telescope
  go = map (second go . unDom) . A.telToList

instance From A.Type where
  type To A.Type = Type
  go = go . A.unEl

instance From A.Term where
  type To A.Term = Term
  go = \case
    -- ** abstractions
    (A.Pi ty ab) -> Pi (pp (absName ab), go (unEl $ unDom ty)) (go $ unEl $ unAbs ab)
    (A.Lam _ ab) -> Lam (pp (absName ab), go (unAbs ab))
    -- ** applications.
    (A.Var i   xs) -> App (Right i)     (go <$> xs)
    (A.Def f   xs) -> App (Left $ pp f) (go <$> xs)
    (A.Con c _ xs) -> App (Left $ pp c) (go <$> xs)
    -- ** other constants
    (A.Lit   x) -> Lit   $ pp x
    (A.Level x) -> Level $ pp x
    (A.Sort  x) -> Sort  $ pp x
    -- ** crash on the rest (should never be encountered)
    t -> error $ "[PANIC] unexpected term: " <> pp t

instance From A.Elim where
  type To A.Elim = Term
  go = \case
    (A.Apply x)        -> go (unArg x)
    (A.Proj _ qn)      -> App (Left $ pp qn) []
    e@(A.IApply _ _ _) -> error $ "[PANIC] unexpected elim: " <> pp e


