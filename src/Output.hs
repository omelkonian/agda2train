{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-- | Defines the structure of the training data, as well as how to translate
-- internal Agda definition to this format.
module Output where

import Control.Arrow ( second )
import Control.Applicative ( (<|>) )
import GHC.Generics ( Generic )
import Data.List ( notElem, elemIndex )
import Data.String ( fromString )
import Data.Aeson
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM

import Agda.Syntax.Common ( unArg )
import qualified Agda.Syntax.Common as A
import qualified Agda.Syntax.Internal as A
import qualified Agda.Syntax.Literal as A
import Agda.Syntax.Internal
  ( QName, absName, qnameName, qnameModule, unAbs, unEl, unDom
  , nameId, conName, dbPatVarIndex, pDom, telToList, telFromList )
import Agda.Syntax.Translation.InternalToAbstract ( NamedClause(..) )
import qualified Agda.TypeChecking.Monad as A
import Agda.TypeChecking.Monad
  ( TCM, MonadTCM, liftTCM, typeOfConst, theDef, defName, getConstInfo
  , reportSDoc, VerboseLevel )

import qualified Agda.Utils.Pretty as P
import Agda.TypeChecking.Pretty
  ( PrettyTCM(..), MonadPretty, fsep, punctuate, braces, parens, Doc )
import qualified Agda.TypeChecking.Pretty as P
  hiding (text)

-- * Types

-- | Name identifiers.
type Name = String
-- | DeBruijn indices.
type DB   = Int
-- | A head of a λ-application can either be a defined name in the global scope,
-- or a DeBruijn index into the local context.
type Head = Either Name DB

-- * Generic constructions

infixr 4 :>; pattern x :> y = Pretty {pretty = x, thing = y}
-- | Bundle a thing with its "pretty" version.
--
-- NB: In JSON format, we follow a /shallow/ encoding with "pretty" being
-- an additional field and "thing" just inlined in the top-level record.
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
  } deriving (Generic, Functor, Foldable, Traversable)
deriving instance Show a => Show (Reduced a)
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
  } deriving Generic
instance ToJSON   TrainData where toJSON    = genericToJSON jsonOpts
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
  } deriving (Generic, Show)
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
  } deriving (Generic, Show, ToJSON, FromJSON)


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
  deriving (Generic, Show, ToJSON, FromJSON)

-- | Function clauses.
data Clause = Clause
  { _telescope :: Telescope
  -- ^ the telescope induced by the clause's context and patterns
  , patterns  :: [Pattern]
  -- ^ the actual patterns of this function clause
  , body      :: Maybe Term
  -- ^ the right hand side of the clause (@Nothing@ for absurd clauses)
  } deriving (Generic, Show)
instance ToJSON   Clause where toJSON    = genericToJSON jsonOpts
instance FromJSON Clause where parseJSON = genericParseJSON jsonOpts

-- | A telescope is a sequence of (named) types, a.k.a. bindings.
type Telescope = [Named (Pretty Type)]
-- | We under-approximate patterns as terms,
-- e.g. losing information about forced patterns.
type Pattern   = Term
-- | Types are the same as terms.
type Type      = Term

-- | The AST of Agda terms.
data Term
  = Pi Bool (Named Term) Term -- ^ e.g. @∀ {A : Set}. A → A@
  | Lam (Named Term)          -- ^ e.g. @λ x. x@
  | App Head [Term]           -- ^ e.g. @f x (x + x)@ or @@0 (λ x. x)@
  | Lit String   -- ^ e.g. @42@ or @"something"@
  | Sort String  -- ^ e.g. @Set@
  | Level String -- ^ e.g. @0ℓ@
  | UnsolvedMeta -- ^ i.e. @{!!}@
  deriving (Generic, Show, FromJSON)

instance ToJSON Term where
  toJSON = \case
    (Pi isDep (n :~ dom) codom) -> object
      [ tag "Pi"
      , "name"     .= toJSON n -- T0D0: remove if "(nothing) (or _)?"
      , "domain"   .= toJSON dom
      , "codomain" .= toJSON codom
      ]
    (Lam (n :~ f)) -> object
      [ tag "Lambda"
      , "abstraction" .= toJSON n
      , "body"        .= toJSON f
      ]
    (App f xs) ->
      if null xs then
        refHead
      else
        object [tag "Application", "head" .= refHead, "arguments" .= toJSON xs]
      where
        refHead = object $ case f of
          (Left n)  -> [tag "ScopeReference", "name"  .= toJSON n]
          (Right i) -> [tag "DeBruijn",       "index" .= toJSON i]
    (Lit s)   -> object [tag "Literal", "literal" .= toJSON s]
    (Sort s)  -> object [tag "Sort",    "sort"   .= toJSON s]
    (Level s) -> object [tag "Level",   "level"  .= toJSON s]
    UnsolvedMeta -> object [tag "UnsolvedMetavariable"]
    where tag s = "tag" .= JSON.String s

-- * Conversion from Agda's internal syntax

-- | Converting between two types @a@ and @b@ under Agda's typechecking monad.
--
-- NB: 'go' is only used internally to de-clutter the recursive calls.
class (~>) a b | a -> b where
  convert, go :: a -> TCM b
  convert = go

instance A.Definition ~> Definition where
  go = go . theDef

instance A.Defn ~> Definition where
  go = \case
    A.AbstractDefn defn -> go defn
    A.Function{..} -> let cls = takeWhile isNotCubical funClauses in
      -- NB: handle funWith and funExtLam
      Function <$> traverse go cls
    A.Datatype{..} -> do
      -- NB: what is a dataClause???
      tys <- fmap unEl <$> traverse typeOfConst dataCons
      ADT <$> traverse go tys
    A.Record{..} -> do
      -- NB: incorporate conHead/namedCon in the future for accuracy
      --     + to solve the issue with private (non-public) fields
      (tel, fs) <- splitAt recPars <$> go recTel
      return $ Record tel (thing . item <$> fs)
    A.Constructor{..} -> do
      let cn = conName conSrcCon
      d <- theDef <$> getConstInfo conData
      return $ case d of
        A.Datatype{..} ->
          let Just ix = elemIndex (unqualify cn) (unqualify <$> dataCons)
          in  Constructor (pp conData) (toInteger ix)
        A.Record{..} -> Constructor (pp conData) 0
    A.Primitive{..}      -> return Primitive
    A.PrimitiveSort{..}  -> return Primitive
    A.Axiom{..}          -> return Postulate
    d@A.DataOrRecSig{..} -> panic "dataOrRecSig" d
    d@A.GeneralizableVar -> panic "generalizable variable" d

instance A.Clause ~> Clause where
  go A.Clause{..} =
    Clause <$> go clauseTel
           <*> traverse go (A.namedThing . unArg <$> namedClausePats)
           -- ^ drop visibility and name information
           <*> traverse go clauseBody

instance A.DeBruijnPattern ~> Pattern where
  go = \case
    A.VarP _ v -> return $ App (Right $ dbPatVarIndex v) []
    A.DotP _ t -> go t
    A.ConP c _ ps -> do
      App (Left $ pp c) <$> traverse go (A.namedThing . unArg <$> ps)
    A.LitP _ lit -> return $ Lit (pp lit)
    A.ProjP _ qn -> return $ App (Left $ pp qn) []
    p@(A.IApplyP _ _ _ _) -> panic "pattern (cubical)" p
    p@(A.DefP _ _ _)      -> panic "pattern (cubical)" p

instance A.Telescope ~> Telescope where
  go = traverse action . A.telToList
    where
    action :: A.Dom (Name, A.Type) -> TCM (Named (Pretty Type))
    action dty = do
      let (n, ty) = unDom dty
      pty <- ppm ty
      ty' <- go ty
      let pdty = prender $ pDom dty $ P.text $ n <> " : " <> prender pty
      return $ n :~ pdty :> ty'

instance A.Type ~> Type where
  go = go . A.unEl

instance A.Term ~> Term where
  go = flip (.) A.unSpine $ \case
    -- ** abstractions
    (A.Pi ty ab) -> do
      let nameUsed = pp (A.domName ty) `notElem` ["_", "(nothing)"]
      ty' <- go (unEl $ unDom ty)
      ab' <- go (unEl $ unAbs ab)
      return $ Pi nameUsed (absName ab :~ ty') ab'
    (A.Lam _ ab) -> do
      ab' <- go (unAbs ab)
      return $ Lam (pp (absName ab) :~ ab')
    -- ** applications
    (A.Var i   xs) -> App (Right i)                   <$> (traverse go xs)
    (A.Def f   xs) -> App (Left $ ppName f)           <$> (traverse go xs)
    (A.Con c _ xs) -> App (Left $ ppName $ conName c) <$> (traverse go xs)
    -- ** other constants
    (A.Lit   x)   -> return $ Lit   $ pp x
    (A.Level x)   -> return $ Level $ pp x
    (A.Sort  x)   -> return $ Sort  $ pp x
    (A.MetaV _ _) -> return UnsolvedMeta
    -- ** there are some occurrences of `DontCare` in the standard library
    (A.DontCare t) -> go t
    -- ** crash on the rest (should never be encountered)
    t@(A.Dummy _ _) -> panic "term" t

instance A.Elim ~> Term where
  go = \case
    (A.Apply x)      -> go (unArg x)
    (A.Proj _ qn)    -> return $ App (Left $ ppName qn) []
    (A.IApply _ _ x) -> go x

-- * Utilities

pp :: P.Pretty a => a -> String
pp = P.prettyShow

ppm :: (MonadPretty m, P.PrettyTCM a) => a -> m Doc
ppm = P.prettyTCM

prender :: Doc -> String
prender = P.renderStyle (P.Style P.OneLineMode 0 0.0)

pinterleave :: (Applicative m, Semigroup (m Doc)) => m Doc -> [m Doc] -> m Doc
pinterleave sep = fsep . punctuate sep

pbindings :: (MonadPretty m, PrettyTCM a) => [(String, a)] -> [m Doc]
pbindings = map $ \(n, ty) -> parens $ ppm n <> " : " <> ppm ty

report :: MonadTCM m => VerboseLevel -> TCM Doc -> m ()
report n x = liftTCM $ reportSDoc "agda2train" n x

panic :: (P.Pretty a, Show a) => String -> a -> b
panic s t = error $
  "[PANIC] unexpected " <> s <> ": " <> pp t <> "\n show: " <> pp (show t)

ppName :: A.QName -> String
ppName qn = pp qn <> "<" <> show (fromEnum $ nameId $ qnameName qn) <> ">"

unqualify :: A.QName -> String
unqualify = pp . qnameName

(\/) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(f \/ g) x = f x || g x

isNotCubical :: A.Clause -> Bool
isNotCubical A.Clause{..}
  | Just (A.Def qn _) <- clauseBody
  = pp (qnameModule qn) /= "Agda.Primitive.Cubical"
  | otherwise
  = True

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
      s -> s
  }

instance P.PrettyTCM A.Definition where
  prettyTCM d = go (theDef d)
   where
    go = \case
      A.AbstractDefn defn -> go defn
      A.Function{..} -> let cls = takeWhile isNotCubical funClauses in
        fsep $ punctuate " |"
             $ ppm . NamedClause (defName d) True <$> cls
      A.Datatype{..} -> do
        tys <- fmap unEl <$> traverse typeOfConst dataCons
        pinterleave " |" $ pbindings $ zip (unqualify <$> dataCons) tys
      A.Record{..} ->
        let (tel, fs) = splitAt recPars $ telToList recTel in
        (if null tel then "" else ppm (telFromList tel) <> " |- ")
          <> (braces $ pinterleave " ;" $ pbindings $ unDom <$> fs)
      A.Constructor{..} -> do
        let cn = conName conSrcCon
        d <- theDef <$> getConstInfo conData
        case d of
          A.Datatype{..} ->
            let Just ix = elemIndex (unqualify cn) (unqualify <$> dataCons)
            in  ppm conData <> "@" <> ppm ix
          A.Record{..} -> ppm conData <> "@0"
      A.Primitive{..}     -> "<Primitive>"
      A.PrimitiveSort{..} -> "<PrimitiveSort>"
      A.Axiom{..}         -> "<Axiom>"
      A.DataOrRecSig{..}  -> "<DataOrRecSig>"
      A.GeneralizableVar  -> "<GeneralizableVar>"
