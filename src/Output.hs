{-# LANGUAGE FlexibleInstances, MultiWayIf, StandaloneDeriving #-}
module Output
  ( Sample(..)
  , FileData, TrainData(..)
  , ScopeEntry, ScopeEntry'(..)
  , convert
  , Pretty(..), Reduced(..), Named(..)
  , pattern (:~), pattern (:>)
  , pp
  )
  where

import Control.Arrow ( second )
import Control.Applicative ( (<|>) )
import GHC.Generics
import Data.List ( notElem )
import Data.Aeson
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM

import Agda.Syntax.Common ( unArg )
import qualified Agda.Syntax.Internal as A
import qualified Agda.Syntax.Literal as A
import Agda.Syntax.Internal ( absName, unAbs, unEl, unDom )

import qualified Agda.Utils.Pretty as P


-- ** JSON options
jsonOpts = defaultOptions
  { omitNothingFields = True
  , fieldLabelModifier = \case
      "scopeGlobal" -> "scope-global"
      "scopeLocal" -> "scope-local"
      "_type" -> "type"
      s -> s
  }

-- ** types

type Name = String
type DB   = Int
type Head = Either Name DB

-- generic constructions

infixr 4 :>; pattern x :> y = Pretty {pretty = x, thing = y}
data Pretty a = Pretty
  { pretty :: String
  , thing  :: a
  } deriving Generic
deriving instance Show a => Show (Pretty a)
instance ToJSON a => ToJSON (Pretty a) where
  toJSON (Pretty{..}) = let pretty' = toJSON pretty in
    case toJSON thing of
      (Object fs)  -> object ("pretty" .= pretty' : KM.toList fs)
      t@(Array xs) -> object ["pretty" .= pretty', "telescope" .= t]
      t            -> object ["pretty" .= pretty', "thing" .= toJSON t]
instance FromJSON a => FromJSON (Pretty a) where
  parseJSON = withObject "Pretty" $ \v -> Pretty
    <$> v .: "pretty"
    <*> (v .: "thing" <|> parseJSON (Object v))

data Reduced a = Reduced
  { simplified :: Maybe a
  , reduced    :: Maybe a
  , normalised :: Maybe a
  , original   :: a
  } deriving (Generic, Functor)
deriving instance Show a => Show (Reduced a)
instance ToJSON a => ToJSON (Reduced a) where
  toJSON r@(Reduced{..})
    | Nothing <- simplified <|> reduced <|> normalised
    = toJSON original
    | otherwise
    = genericToJSON jsonOpts r
instance FromJSON a => FromJSON (Reduced a) where
  parseJSON = withObject "Object" $ \v -> Reduced
    <$> v .:? "simplified"
    <*> v .:? "reduced"
    <*> v .:? "normalised"
    <*> (v .: "original" <|> parseJSON (Object v))

infixr 4 :~; pattern x :~ y = Named {name = x, item = y}
data Named a = Named
  { name :: Name
  , item :: a
  } deriving (Generic, Show)
instance ToJSON a => ToJSON (Named a) where
  toJSON (Named{..}) = let name' = toJSON name in
    case toJSON item of
      (Object fs) -> object ("name" .= name' : KM.toList fs)
      x           -> object ["name" .= name', "item" .= toJSON x]
instance FromJSON a => FromJSON (Named a) where
  parseJSON = withObject "Named" $ \v -> Named
    <$> v .: "name"
    <*> (v .: "item" <|> parseJSON (Object v))

-- concrete types (very close to JSON format)

type FileData = Named TrainData
data TrainData = TrainData
  { scopeGlobal :: [ScopeEntry] -- these will not contain any holes
  , scopeLocal  :: [ScopeEntry]
  } deriving Generic
instance ToJSON TrainData where
  toJSON = genericToJSON jsonOpts
instance FromJSON TrainData where
  parseJSON = genericParseJSON jsonOpts

type ScopeEntry = Named ScopeEntry'
data ScopeEntry' = ScopeEntry
  { _type      :: Pretty (Reduced Type)
  , definition :: Maybe (Pretty Term)
  , holes      :: Maybe [Sample]
  } deriving (Generic, Show)
instance ToJSON   ScopeEntry' where toJSON    = genericToJSON    jsonOpts
instance FromJSON ScopeEntry' where parseJSON = genericParseJSON jsonOpts

data Sample = Sample
  { ctx      :: Pretty Telescope
  , goal     :: Pretty (Reduced Type)
  , term     :: Pretty (Reduced Term)
  , premises :: [Name]
  } deriving (Generic, Show)
    deriving (ToJSON, FromJSON) via Generically Sample

type Telescope = [Named (Pretty Type)]
type Type = Term

data Term
  = Pi Bool (Named Term) Term -- ^ e.g. `∀ {A : Set}. A → A`
  | Lam (Named Term)          -- ^ e.g. `λ x. x`
  | App Head [Term]           -- ^ e.g. `f x (x + x)` or `@0 (λ x. x)`
  | ADT [Type]                -- ^ e.g. `data ℕ : Set where zero : ℕ; suc : ...`
  | Constructor Name Int      -- ^ e.g. `(ℕ, 0) ~ zero` or `(ℕ, 1) ~ suc`
  | Record [Type]             -- ^ e.g. `record X where field x : ℕ; y : ℕ`
  | Function [Term]           -- ^ e.g. `f [] = []; f (x ∷ xs) = ...`
  | Lit String | Sort String | Level String -- ^ e.g. Set/42/"sth",0ℓ,...
  deriving (Generic, Show)
  deriving (FromJSON) via Generically Term

instance ToJSON Term where
  toJSON = \case
    (Pi isDep (n :~ dom) codom) -> object
      [ tag "Pi"
      , "name"     .= toJSON n
      , "domain"   .= toJSON dom
      , "codomain" .= toJSON codom
      ]
    (Lam (n :~ f)) -> object
      [ tag "Lambda"
      , "body"        .= toJSON f
      , "abstraction" .= toJSON n
      ]
    (Function cls) -> object
      [tag "Function", "clauses" .= toJSON cls]
    (Record tys) -> object
      [tag "Record", "fields" .= toJSON tys]
    (ADT cs) -> object
      [tag "ADT", "variants" .= toJSON cs]
    (Constructor n i) -> object
      [ tag "Constructor"
      , "reference" .= toJSON n
      , "variant"   .= toJSON i
      ]
    (App f xs) ->
      if null xs then
        refHead
      else
        object [tag "Application", "head" .= refHead, "arguments" .= toJSON xs]
      where
        refHead = object $ case f of
          (Left n)  -> [tag "ScopeReference", "name"  .= toJSON n]
          (Right i) -> [tag "deBruijn",       "index" .= toJSON i]
    (Lit s)   -> object [tag "Literal", "literal" .= toJSON s]
    (Sort s)  -> object [tag "Sort",    "sort"   .= toJSON s]
    (Level s) -> object [tag "Level",   "level"  .= toJSON s]
    where tag s = "tag" .= JSON.String s

testJSON :: IO ()
testJSON = do
  let ty = Pi True ("A" :~ Sort "Set")
         $ Pi False ("_" :~ App (Left "A") [])
         $ App (Left "A") []
      t  = Lam ("a" :~ App (Right 0) [])
  encodeFile "type.json" ty >> encodeFile "term.json" t
  Just ty <- decodeFileStrict "type.json" :: IO (Maybe Term)
  putStrLn $ "ty: " <> show ty
  Just t <- decodeFileStrict "term.json" :: IO (Maybe Term)
  putStrLn $ "t: " <> show t

-- ** conversion from Agda's internal syntax

class From a where
 type To a
 convert, go :: a -> To a
 convert = go

instance From A.Telescope where
  type To A.Telescope = Telescope
  go = map (\dty -> let (n, ty) = unDom dty in n :~ pp dty :> go ty) . A.telToList

instance From A.Type where
  type To A.Type = Type
  go = go . A.unEl

instance From A.Term where
  type To A.Term = Term
  go = \case
    -- ** abstractions
    (A.Pi ty ab) -> Pi (pp (A.domName ty) `notElem` ["_", "(nothing)"])
                       (absName ab :~ go (unEl $ unDom ty))
                       (go $ unEl $ unAbs ab)
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
    (A.Dummy s xs) -> let xs' = go <$> xs in case s of
      "⊜" -> Function xs'
      "⊕" -> ADT xs'
      "⊗" -> Record xs'
      "⊙" -> if | [App (Left n) [], Lit i] <- xs'
                -> Constructor n (read i)
                | otherwise
                -> error $ "[convert] malformed constructor: " <> show xs'
      _ -> App (Left (s <> s)) xs'
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
