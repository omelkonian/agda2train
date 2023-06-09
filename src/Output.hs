{-# LANGUAGE FlexibleInstances #-}
module Output
  ( Sample(..)
  , TrainData(..)
  , ScopeEntry
  , convert
  , Reduced(..)
  , pattern (:~), pattern (:>), pattern (:=)
  , pp
  )
  where

import Control.Arrow ( second )
import GHC.Generics
import Data.Aeson

import Agda.Syntax.Common ( unArg )
import qualified Agda.Syntax.Internal as A
import Agda.Syntax.Internal ( absName, unAbs, unEl, unDom )

import qualified Agda.Utils.Pretty as P

-- ** types

type Name = String
type DB   = Int
type Head = Either Name DB

infixr 4 :>; pattern x :> y = Pretty {pretty = x, thing = y}
data Pretty a = Pretty
  { pretty :: String
  , thing  :: a
  } deriving Generic
instance ToJSON a => ToJSON (Pretty a)
instance FromJSON a => FromJSON (Pretty a)

data Reduced a = Reduced
  { simplified :: Maybe a
  , reduced    :: Maybe a
  , normalised :: Maybe a
  , original   :: a
  } deriving (Generic, Functor)
instance ToJSON a => ToJSON (Reduced a)
instance FromJSON a => FromJSON (Reduced a)

infixr 4 :~; pattern x :~ y = Named {name = x, item = y}
data Named a = Named
  { name :: Name
  , item :: a
  } deriving (Generic, Show)
instance ToJSON a => ToJSON (Named a)
instance FromJSON a => FromJSON (Named a)

data TrainData = TrainData
  { scope   :: Named [ScopeEntry]
  , samples :: [Sample]
  } deriving Generic
    deriving (ToJSON, FromJSON) via Generically TrainData

infixr 4 :=; pattern x := y = (x, y)
type ScopeEntry = Named (Pretty (Reduced Type), Maybe (Pretty Term))

data Sample = Sample
  { ctx   :: Pretty Telescope
  , goal  :: Pretty (Reduced Type)
  , term  :: Pretty (Reduced Term)
  , namesUsed :: [Name]
  } deriving Generic
    deriving (ToJSON, FromJSON) via Generically Sample

type Telescope = [Named Type]

data Term
  = Pi (Named Term) Term -- ^ e.g. `∀ {A : Set}. A → A`
  | Lam (Named Term)     -- ^ e.g. `λ x. x`
  | App Head [Term]      -- ^ e.g. `f x (x + x)` or `@0 (λ x. x)`
  | Lit String | Sort String | Level String -- ^ e.g. Set/42/"sth",0ℓ,...
  deriving (Generic, Show)
  deriving (ToJSON, FromJSON) via Generically Term

type Type = Term

testJSON :: IO ()
testJSON = do
  let ty = Pi ("A" :~ Sort "Set") (Pi ("_" :~ App (Left "A") []) (App (Left "A") []))
      t = Lam ("a" :~ App (Right 0) [])
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

instance From A.Telescope where
  type To A.Telescope = Telescope
  go = map (uncurry (:~) . second go . unDom) . A.telToList

instance From A.Type where
  type To A.Type = Type
  go = go . A.unEl

instance From A.Term where
  type To A.Term = Term
  go = \case
    -- ** abstractions
    (A.Pi ty ab) -> Pi (pp (absName ab) :~ go (unEl $ unDom ty)) (go $ unEl $ unAbs ab)
    (A.Lam _ ab) -> Lam (pp (absName ab) :~ go (unAbs ab))
    -- ** applications.
    (A.Var i   xs) -> App (Right i)     (go <$> xs)
    (A.Def f   xs) -> App (Left $ pp f) (go <$> xs)
    (A.Con c _ xs) -> App (Left $ pp c) (go <$> xs)
    -- ** other constants
    (A.Lit   x) -> Lit   $ pp x
    (A.Level x) -> Level $ pp x
    (A.Sort  x) -> Sort  $ pp x
    -- ** there are some occurrences of `DontCare` in the standard library
    (A.DontCare t) -> go t
    (A.Dummy s xs) -> App (Left s) (go <$> xs)
    -- ** crash on the rest (should never be encountered)
    t@(A.MetaV _ _) -> panic "term" t

instance From A.Elim where
  type To A.Elim = Term
  go = \case
    (A.Apply x)      -> go (unArg x)
    (A.Proj _ qn)    -> App (Left $ pp qn) []
    (A.IApply _ _ x) -> go x

-- ** utilities

pp :: P.Pretty a => a -> String
pp = P.prettyShow

panic :: (P.Pretty a, Show a) => String -> a -> b
panic s t = error $
  "[PANIC] unexpected " <> s <> ": " <> pp t <> "\n show: " <> pp (show t)
