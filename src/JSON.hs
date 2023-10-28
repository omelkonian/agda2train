{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-- | Defines the structure of the training data format.
module JSON where

import GHC.Generics ( Generic )

import Control.Applicative ( (<|>), liftA2 )

import qualified Data.ByteString.Lazy as BL
import Data.Aeson hiding ( encode )
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Encode.Pretty
  ( encodePretty', Config(..), defConfig, Indent(..), keyOrder )


-- * Types

-- | Name identifiers.
type Name = String
-- | DeBruijn indices.
type DB   = Int
-- | A head of a λ-application can either be a defined name in the global scope,
-- or a DeBruijn index into the local context.
type Head = Either Name DB
pattern Ref x = Left x; pattern DB x = Right x

-- * Generic constructions

infixr 4 :>; pattern x :> y = Pretty {pretty = x, thing = y}
-- | Bundle a thing with its "pretty" version.
--
-- NB: In JSON format, we follow a /shallow/ encoding with "pretty" being
-- an additional field and "thing" just inlined in the top-level record.
data Pretty a = Pretty
  { pretty :: String
  , thing  :: a
  } deriving (Generic, Show, Eq)
instance ToJSON a => ToJSON (Pretty a) where
  toJSON (Pretty{..}) = let pretty' = toJSON pretty in
    case toJSON thing of
      (Object fs)  -> object ("pretty" .= pretty' : KM.toList fs)
      t@(Array xs) -> object ["pretty" .= pretty', "telescope" .= t]
      t            -> object ["pretty" .= pretty', "thing"     .= t]
instance FromJSON a => FromJSON (Pretty a) where
  parseJSON = withObject "Pretty" $ \v -> Pretty
    <$> v .: "pretty"
    <*> (v .: "telescope" <|> v .: "thing" <|> parseJSON (Object v))

-- | Bundle a term with (several of) its normalised forms.
--
-- We do not repeat identical elements in subsequent evaluations
-- (in the order simp/red/norm) and some fields may not be populated due to
-- the evaluation taking to long and leading to a timeout (currently 2 seconds).
--
-- NB: Like 'Named', encoded in a /shallow/ JSON.
data Reduced a = Reduced
  { original   :: a
  , simplified :: Maybe a
  , reduced    :: Maybe a
  , normalised :: Maybe a
  } deriving (Generic, Show, Eq, Functor, Foldable, Traversable)
instance ToJSON a => ToJSON (Reduced a) where
  toJSON r@(Reduced{..})
    | Nothing <- simplified <|> reduced <|> normalised
    = toJSON original
    | otherwise
    = genericToJSON jsonOpts r
instance FromJSON a => FromJSON (Reduced a) where
  parseJSON = withObject "Object" $ \v -> Reduced
    <$> (v .: "original" <|> parseJSON (Object v))
    <*> v .:? "simplified"
    <*> v .:? "reduced"
    <*> v .:? "normalised"

infixr 4 :~; pattern x :~ y = Named {name = x, item = y}

-- | Bundle a thing with its name.
--
-- NB: Like 'Named' and 'Reduced', encoded in a /shallow/ JSON.
data Named a = Named
  { name :: Name
  , item :: a
  } deriving (Generic, Show, Eq, Functor)
instance ToJSON a => ToJSON (Named a) where
  toJSON (Named{..}) = let name' = toJSON name in
    case toJSON item of
      (Object fs) -> object ("name" .= name' : KM.toList fs)
      x           -> object ["name" .= name', "item" .= toJSON x]
instance FromJSON a => FromJSON (Named a) where
  parseJSON = withObject "Named" $ \v -> Named
    <$> v .: "name"
    <*> (v .: "item" <|> parseJSON (Object v))

-- * Concrete types (~ JSON schema)

-- | Data for a file include the filename and its training data.
type FileData = Named TrainData
-- | The training data for a module, divided into three parts.
data TrainData = TrainData
  { scopeGlobal  :: [ScopeEntry]
  -- ^ The /global/ scope, giving the types and definitions of all @import@ statements.
  --
  -- NB: does not contain any /holes/ for subterms.
  , scopeLocal   :: [ScopeEntry]
  -- ^ The /local/ scope, containing the types, definitions, and training data
  -- for each of this module's definitions.
  , scopePrivate :: Maybe [ScopeEntry]
  -- ^ The /private/ scope, containing private definitions not exported to the public,
  -- as well as system-generated definitions stemming from @where@ or @with@.
  } deriving (Generic, Show, Eq)
instance ToJSON   TrainData where toJSON    = genericToJSON    jsonOpts
instance FromJSON TrainData where parseJSON = genericParseJSON jsonOpts

-- | Every 'ScopeEntry'' is /named/.
type ScopeEntry = Named ScopeEntry'
-- | An entry in the scope: type, definitions, and holes.
data ScopeEntry' = ScopeEntry
  { _type      :: Pretty (Reduced Type)
  -- ^ The entry's type.
  , definition :: Maybe (Pretty Definition)
  -- ^ The actual body of this entry's definition.
  , holes      :: Maybe [Sample]
  -- ^ Training data for each of the subterms in this entry's 'definition'.
  } deriving (Generic, Show, Eq)
instance ToJSON   ScopeEntry' where toJSON    = genericToJSON    jsonOpts
instance FromJSON ScopeEntry' where parseJSON = genericParseJSON jsonOpts

-- | The training sample for each sub-hole.
data Sample = Sample
  { ctx      :: Pretty Telescope
  -- ^ The current context, as a /binding telescope/.
  , goal     :: Pretty (Reduced Type)
  -- ^ The current goal, i.e. type of the sub-term.
  --
  -- NB: DeBruijn indices here refer to the 'ctx'.
  , term     :: Pretty (Reduced Term)
  -- ^ The term that successfully fills the current 'goal'.
  , premises :: [Name]
  -- ^ Definitions used in this "proof", intended to be used for /premise selection/.
  } deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | Agda definitions: datatypes, records, functions, postulates and primitives.
data Definition
  = ADT {variants :: [Type]}
  -- ^ e.g.
  -- data ℕ : Set where
  --   zero : ℕ
  --   suc  : ℕ → ℕ
  | Constructor {reference :: Name, variant :: Integer}
  -- ^ e.g. `(ℕ, 0) ~ zero` or `(ℕ, 1) ~ suc`
  | Record {telescope :: Telescope, fields :: [Type]}
  -- ^ e.g.
  -- record X : Set where
  --   field x : ℕ
  --         y : ℕ
  | Function {clauses :: [Clause]}
  -- ^ e.g.
  -- f []       = []
  -- f (x ∷ xs) = x ∷ x ∷ xs
  | Postulate {}
  -- ^ e.g. `postulate pred : ℕ → ℕ`
  | Primitive {}
  -- ^ e.g. `primitive primShowNat : ℕ → String`
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | Function clauses.
data Clause = Clause
  { _telescope :: Telescope
  -- ^ the telescope induced by the clause's context and patterns
  , patterns  :: [Pattern]
  -- ^ the actual patterns of this function clause
  , body      :: Maybe Term
  -- ^ the right hand side of the clause (@Nothing@ for absurd clauses)
  } deriving (Generic, Show, Eq)
instance ToJSON   Clause where toJSON    = genericToJSON    jsonOpts
instance FromJSON Clause where parseJSON = genericParseJSON jsonOpts

-- | A telescope is a sequence of (named) types, a.k.a. bindings.
type Telescope = [Named (Pretty Type)]
-- | We under-approximate patterns as terms,
-- e.g. losing information about forced patterns.
type Pattern   = Term
-- | Types are the same as terms, but also accompanied by their "dependency level".
data Type = Type
  { tyTerm   :: Term
  , depLevel :: Maybe Int
  } deriving (Generic, Show, Eq)

instance ToJSON Type where
  toJSON (Type{..}) = let dl = toJSON depLevel in
    case toJSON tyTerm of
      (Object fs) -> object ("dependency-level" .= dl : KM.toList fs)
      _           -> error "[toJSON<Type>] term is not a JSON object"
instance FromJSON Type where
  parseJSON = withObject "Type" $ \v -> Type
    <$> parseJSON (Object v)
    <*> v .:? "dependency-level"

-- | The AST of Agda terms.
data Term
  = Pi Bool (Named Term) Term -- ^ e.g. @∀ {A : Set}. A → A@
  | Lam (Named Term)          -- ^ e.g. @λ x. x@
  | App Head [Term]           -- ^ e.g. @f x (x + x)@ or @@0 (λ x. x)@
  | Lit String   -- ^ e.g. @42@ or @"something"@
  | Sort String  -- ^ e.g. @Set@
  | Level String -- ^ e.g. @0ℓ@
  | UnsolvedMeta -- ^ i.e. @{!!}@
  deriving (Generic, Show, Eq)

instance {-# OVERLAPPING #-} ToJSON Head where
  toJSON = object . \case
    (Ref n)  -> [tag "ScopeReference", "ref-name"  .= toJSON n]
    (DB i) -> [tag "DeBruijn",       "index" .= toJSON i]
    where tag s = "tag" .= JSON.String s

instance {-# OVERLAPPING #-} FromJSON Head where
  parseJSON = withObject "Head" $ \o -> o .: "tag" >>= \case
    String "ScopeReference" -> Ref <$> o .: "ref-name"
    String "DeBruijn"       -> DB  <$> o .: "index"
    tag -> fail $ "Cannot parse Head: unexpected \"tag\" field " <> show tag

instance ToJSON Term where
  toJSON = \case
    (Pi isDep (n :~ dom) codom) -> object
      [ tag "Pi"
      , "bound-name" .= toJSON n -- T0D0: remove if "(nothing) (or _)?"
      , "domain"    .= toJSON dom
      , "codomain"  .= toJSON codom
      ]
    (Lam (n :~ f)) -> object
      [ tag "Lambda"
      , "abstraction" .= toJSON n
      , "body"        .= toJSON f
      ]
    (App hd xs) -> let hd' = toJSON hd in
      if null xs then
        hd'
      else
        object [tag "Application", "head" .= hd', "arguments" .= toJSON xs]
    (Lit s)   -> object [tag "Literal", "literal" .= toJSON s]
    (Sort s)  -> object [tag "Sort",    "sort"   .= toJSON s]
    (Level s) -> object [tag "Level",   "level"  .= toJSON s]
    UnsolvedMeta -> object [tag "UnsolvedMetavariable"]
    where tag s = "tag" .= JSON.String s

instance FromJSON Term where
  parseJSON = withObject "Term" $ \o -> o .: "tag" >>= \case
    String "Pi" -> Pi True <$> liftA2 (:~) (o .: "bound-name") (o .: "domain")
                           <*> o .: "codomain"
      -- T0D0: also serialise `isDep`
    String "Lambda" -> Lam <$> liftA2 (:~) (o .: "abstraction") (o .: "body")
    String "Application"    -> App <$> o .: "head" <*> o .: "arguments"
    String "ScopeReference" -> flip App [] . Ref <$> o .: "ref-name"
    String "DeBruijn"       -> flip App [] . DB  <$> o .: "index"
    String "Literal"  -> Lit   <$> o .: "literal"
    String "Sort"     -> Sort  <$> o .: "sort"
    String "Level"    -> Level <$> o .: "level"
    String "UnsolvedMetavariable" -> pure UnsolvedMeta
    tag -> fail $ "Cannot parse Term: unexpected \"tag\" field " <> show tag

-- ** JSON encoding

-- | Configure JSON to omit empty (optional) fields and switch
-- from camelCase to kebab-case.
jsonOpts :: JSON.Options
jsonOpts = defaultOptions
  { omitNothingFields = True
  , fieldLabelModifier = \case
      ('_' : s) -> s
      "scopeGlobal"  -> "scope-global"
      "scopeLocal"   -> "scope-local"
      "scopePrivate" -> "scope-private"
      "refName"      -> "ref-name"
      "boundName"    -> "bound-name"
      s -> s
  }

-- | Uses "Aeson.Pretty" to order the JSON fields.
encode :: ToJSON a => a -> BL.ByteString
encode = encodePretty' $ defConfig
  { confIndent = Spaces 2
  , confCompare = keyOrder
      [ "pretty"
      , "name"
      , "dependency-level"
      , "tag"
      , "original", "simplified", "reduced", "normalised"
      , "telescope", "patterns", "fields"
      , "bound-name" , "domain", "codomain"
      , "abstraction", "body"
      , "sort", "level", "literal"
      , "head", "arguments"
      , "variants", "reference", "variant"
      , "ref-name", "index"
      , "scope-global", "scope-local", "scope-private"
      , "type", "definition", "holes"
      , "ctx", "goal", "term", "premises"
      ]
  }

encodeFile :: ToJSON a => FilePath -> a -> IO ()
encodeFile = \fn -> BL.writeFile fn . encode

