{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Output
  ( Sample(..)
  , FileData, TrainData(..)
  , ScopeEntry, ScopeEntry'(..)
  , Pretty(..), Reduced(..), Named(..)
  , pattern (:~), pattern (:>)
  , convert
  , pp, ppName, ppm, prender, report
  , unqualify
  , (\/)
  )
  where

import Control.Arrow ( second )
import Control.Applicative ( (<|>) )
import GHC.Generics ( Generic, Generically(..) )
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
  , nameId, conName, dbPatVarIndex, pDom, telToList )
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
instance ToJSON   TrainData where toJSON    = genericToJSON jsonOpts
instance FromJSON TrainData where parseJSON = genericParseJSON jsonOpts

type ScopeEntry = Named ScopeEntry'
data ScopeEntry' = ScopeEntry
  { _type      :: Pretty (Reduced Type)
  , definition :: Maybe (Pretty Definition)
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


data Definition
  = ADT {variants :: [Type]}
  -- ^ e.g.
  -- data ℕ : Set where
  --   zero : ℕ
  --   suc  : ℕ → ℕ
  | Constructor {reference :: Name, variant :: Integer}
  -- ^ e.g. `(ℕ, 0) ~ zero` or `(ℕ, 1) ~ suc`
  | Record {fields :: [Type]}
  -- ^ e.g.
  -- record X : Set where
  --   field x : ℕ
  --         y : ℕ
  | Function {clauses :: [Clause]}
  -- ^ e.g.
  -- f []       = []
  -- f (x ∷ xs) = x ∷ x ∷ xs
  | Postulate {}
  | Primitive {primName :: String}
  deriving (Generic, Show, ToJSON, FromJSON)

data Clause = Clause
  { telescope :: Telescope
  , patterns  :: [Pattern]
  , body      :: Maybe Term -- ^ `Nothing` for absurd clauses
  } deriving (Generic, Show)
instance ToJSON   Clause where toJSON    = genericToJSON jsonOpts
instance FromJSON Clause where parseJSON = genericParseJSON jsonOpts

type Telescope = [Named (Pretty Type)]
type Pattern   = Term
type Type      = Term

data Term
  = Pi Bool (Named Term) Term -- ^ e.g. `∀ {A : Set}. A → A`
  | Lam (Named Term)          -- ^ e.g. `λ x. x`
  | App Head [Term]           -- ^ e.g. `f x (x + x)` or `@0 (λ x. x)`
  | Lit String | Sort String | Level String -- ^ e.g. Set/42/"sth",0ℓ,...
  deriving (Generic, Show)
  deriving FromJSON via Generically Term

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
    -- NB: handle parameterized modules (c.f. recClause/recTel/recPars
    -- NB: maybe incorporate conHead/namedCon in the future for accuracy
      let tys = snd . unDom <$> drop recPars (A.telToList recTel)
      Record <$> traverse go tys
    A.Constructor{..} -> do
      let cn = conName conSrcCon
      d <- theDef <$> getConstInfo conData
      return $ case d of
        A.Datatype{..} ->
          let Just ix = elemIndex (unqualify cn) (unqualify <$> dataCons)
          in  Constructor (pp conData) (toInteger ix)
        A.Record{..} -> Constructor (pp conData) 0
    A.Primitive{..}      -> return $ Primitive primName
    A.PrimitiveSort{..}  -> return $ Primitive primSortName
    A.Axiom{..}          -> return $ Postulate
    d@A.DataOrRecSig{..} -> panic "dataOrRecSig" d
    d@A.GeneralizableVar -> panic "generalizable variable" d

instance A.Clause ~> Clause where
  go A.Clause{..} = do
    tel <- go clauseTel
    ps  <- traverse go (A.namedThing . unArg <$> namedClausePats)
           -- ^ drop visibility and name information
    t   <- traverse go clauseBody
    return $ Clause
      { telescope = tel
      , patterns  = ps
      , body      = t }

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
    (A.Lit   x) -> return $ Lit   $ pp x
    (A.Level x) -> return $ Level $ pp x
    (A.Sort  x) -> return $ Sort  $ pp x
    -- ** there are some occurrences of `DontCare` in the standard library
    (A.DontCare t) -> go t
    -- ** crash on the rest (should never be encountered)
    t@(A.Dummy _ _) -> panic "term" t
    t@(A.MetaV _ _) -> panic "term" t

instance A.Elim ~> Term where
  go = \case
    (A.Apply x)      -> go (unArg x)
    (A.Proj _ qn)    -> return $ App (Left $ ppName qn) []
    (A.IApply _ _ x) -> go x

-- ** utilities

pp :: P.Pretty a => a -> String
pp = P.prettyShow

ppm :: (MonadPretty m, P.PrettyTCM a) => a -> m Doc
ppm = P.prettyTCM

prender :: Doc -> String
prender = P.renderStyle (P.Style P.OneLineMode 0 0.0)

pinterleave :: (Applicative m, Semigroup (m Doc)) => m Doc -> [m Doc] -> m Doc
pinterleave sep = fsep . punctuate sep

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
        pinterleave " |" $ map showNamedTy (zip (unqualify <$> dataCons) tys)
      A.Record{..} -> do
        let tys = unDom <$> drop recPars (telToList recTel)
        braces $ pinterleave " ;" $ map showNamedTy tys
      A.Constructor{..} -> do
        let cn = conName conSrcCon
        d <- theDef <$> getConstInfo conData
        case d of
          A.Datatype{..} ->
            let Just ix = elemIndex (unqualify cn) (unqualify <$> dataCons)
            in  ppm conData <> "@" <> ppm ix
          A.Record{..} -> ppm conData <> "@0"
      A.Primitive{..}     -> "<Primitive> " <> fromString primName
      A.PrimitiveSort{..} -> "<PrimitiveSort> " <> fromString primSortName
      A.Axiom{..}         -> "<Axiom>"
      A.DataOrRecSig{..}  -> "<DataOrRecSig>"
      A.GeneralizableVar  -> "<GeneralizableVar>"

    showNamedTy :: (MonadPretty m, PrettyTCM a) => (String, a) -> m Doc
    showNamedTy = \(n, ty) -> parens $ ppm n <> " : " <> ppm ty


