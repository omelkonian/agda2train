{-# LANGUAGE TypeApplications #-}
-- | Store the JSON information in an SQL database for quick lookups.
--
-- NB: "training" this model by feeding samples merely consists of adding DB rows
module Main where

import GHC.Generics

import System.Environment ( getArgs )
import qualified Data.HashMap.Strict as M
import Data.Maybe ( fromMaybe )
import Control.Monad ( forM_ )
import Control.Arrow ( first )

import Data.Aeson
  ( ToJSON(..),   genericToJSON
  , FromJSON(..), genericParseJSON
  , eitherDecodeFileStrict' )

import Data.Hashable ( Hashable, hash )

-- import Database.Persist.TH

import Agda.Utils.Either ( caseEitherM )

import ToTrain ( names )
import Output hiding ( ScopeEntry, ScopeEntry' )
import qualified Output as O

data ScopeEntry' = ScopeEntry
  { _type      :: Type
  , definition :: Maybe Definition
  } deriving (Generic, Show, Eq, Hashable)
instance ToJSON   ScopeEntry' where toJSON    = genericToJSON    jsonOpts
instance FromJSON ScopeEntry' where parseJSON = genericParseJSON jsonOpts
type ScopeEntry = Named ScopeEntry'
data Context = Context
  { scope   :: [ScopeEntry]
  , context :: [Type]
  , goal    :: Type
  } deriving (Generic, Show, Eq, Hashable)
instance ToJSON   Context where toJSON    = genericToJSON    jsonOpts
instance FromJSON Context where parseJSON = genericParseJSON jsonOpts
type Premises = [String]

type Key'  = Context
type Key   = Int -- hash of `Context`
type Value = Premises
type Database = M.HashMap Key Value

instance Hashable a => Hashable (Named a)
instance Hashable a => Hashable (Pretty a)
instance Hashable a => Hashable (Reduced a)
instance Hashable O.ScopeEntry'; instance Hashable Sample
instance Hashable Definition; instance Hashable Clause; instance Hashable Term

fileSamples :: FileData -> [(Key', Value)]
fileSamples (Named{item = TrainData{..}}) =
  concatMap (map go . fromMaybe [] . holes . item) scopeLocal
  where
  usedIn :: Name -> Term -> Bool
  usedIn n = \case
    Pi _ (n' :~ ty) t -> n `usedIn` ty || ((n' /= n) && n `usedIn` t)
    Lam (n' :~ t) -> (n' /= n) && n `usedIn` t
    App hd ts -> case hd of {Ref n' -> n' == n; DB _ -> False}
              || any (n `usedIn`) ts
    _ -> False

  go :: Sample -> (Key', Value)
  go Sample{..} = Context
    { scope   = fmap bareScope
                 <$> scopeGlobal
                 <> onlyRelevant scopeLocal
                 <> onlyRelevant (fromMaybe [] scopePrivate)
    , context = thing . item <$> thing (ctx)
    , goal    = original (thing goal)
    } .-> premises
    where (.->) = (,)
          onlyRelevant = filter $ (`usedIn` original (thing term)) . name
          bareScope :: O.ScopeEntry' -> ScopeEntry'
          bareScope O.ScopeEntry{..} = ScopeEntry
            { _type = original (thing _type)
            , definition = thing <$> definition
            }

main :: IO ()
main = getArgs >>= \case
  ("add" : dbJson : jsonFns) -> forM_ jsonFns $ \jsonFn -> do
    caseEitherM (eitherDecodeFileStrict' jsonFn) fail $ \fd -> do
      -- putStrLn "** File Data" >> print fd
      let samples = fileSamples fd
      putStrLn "** Samples" -- >> print samples
      forM_ samples $ \(k, _) -> do
        putStrLn "------" >> print k
        putStrLn "# " >> print (hash k)
      let db = M.fromList (first hash <$> samples)
      putStrLn "** Database" >> print db
      putStrLn "serializing database into a .json file"
      encodeFile dbJson db
  ("query" : dbJson : ctxJson : []) ->
    caseEitherM (eitherDecodeFileStrict' @Database dbJson) fail $ \db -> do
      putStrLn "** Database" >> print db
      caseEitherM (eitherDecodeFileStrict' @Context ctxJson) fail $ \ctx -> do
        putStrLn "** Context" >> print ctx
        let hctx = hash ctx
        putStrLn "# " >> print hctx
        case M.lookup hctx db of
          Just premises -> putStrLn "** Premises" >> print premises
          Nothing       -> putStrLn "NOT TRAINED ON THIS CONTEXT!"
  args -> fail
    $ "Usage: agda2train-db add <DB_JSON> <JSON>* \n"
   <> "    or agda2train-db query <CONTEXT_JSON>\n\n"
   <> "User entered: agda2train-db " <> unwords args
