{-# LANGUAGE DeriveGeneric #-}
module Output where

import Data.Char ( toUpper )

import GHC.Generics
import Data.Aeson

import qualified Agda.Syntax.Common as A
import Agda.Syntax.Common ( unArg )
import qualified Agda.Syntax.Internal as A
import Agda.Syntax.Internal ( absName, unAbs, unEl, unDom )
import qualified Agda.Syntax.Internal.Elim as A

import Agda.Utils.Pretty

type Name = String
type DB   = Int
type Head = Either Name DB

data Term
  = Pi (Name, Term) Term -- ^ e.g. `∀ {A : Set}. A → A`
  | Lam (Name, Term)     -- ^ e.g. `λ x. x`
  | App Head [Term]      -- ^ e.g. `f x (x + x)` or `@0 (λ x. x)`
  | Lit String | Sort String | Level String -- ^ e.g. Set/42/"sth",0ℓ,...
  deriving (Generic, Show)

pp :: Pretty a => a -> String
pp = prettyShow

simplify :: A.Term -> Term
simplify = \case
  -- ** abstractions
  (A.Pi ty ab) -> Pi (pp (absName ab), simplify (unEl $ unDom ty)) (simplify $ unEl $ unAbs ab)
  (A.Lam _ ab) -> Lam (pp (absName ab), simplify (unAbs ab))
  -- ** applications.
  (A.Var i   xs) -> App (Right i)     (go <$> xs)
  (A.Def f   xs) -> App (Left $ pp f) (go <$> xs)
  (A.Con c _ xs) -> App (Left $ pp c) (go <$> xs)
  -- ** other constants
  (A.Lit   x) -> Lit   $ pp x
  (A.Level x) -> Level $ pp x
  (A.Sort  x) -> Sort  $ pp x
  -- ** crash on the rest (should never be encountered)
  t -> error $ "[PANIC] unexpected term: " <> prettyShow t
  where
    go :: A.Elim -> Term
    go = \case
      (A.Apply x) -> simplify (unArg x)
      (A.Proj _ qn) -> App (Left $ pp qn) []
      e@(A.IApply _ _ _) -> error $ "[PANIC] unexpected elim: " <> pp e

customOptions :: Options
customOptions = defaultOptions
  -- { fieldLabelModifier      = id
  -- , constructorTagModifier  = id
  -- , allNullaryToStringTag   = True
  -- , omitNothingFields       = False
  -- , sumEncoding             = defaultTaggedObject
  -- , unwrapUnaryRecords      = False
  -- , tagSingleConstructors   = False
  -- , rejectUnknownFields     = False
  -- }

instance ToJSON Term where
  toEncoding = genericToEncoding customOptions
instance FromJSON Term

testJSON :: IO ()
testJSON = do
  let ty = Pi ("A", Sort "Set") (Pi ("_", App (Left "A") []) (App (Left "A") []))
      t = Lam ("a", App (Right 0) [])
  encodeFile "type.json" ty >> encodeFile "term.json" t
  Just ty <- decodeFileStrict "type.json" :: IO (Maybe Term)
  putStrLn $ "ty: " <> show ty
  Just t <- decodeFileStrict "term.json" :: IO (Maybe Term)
  putStrLn $ "t: " <> show t
