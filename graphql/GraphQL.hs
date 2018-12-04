{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GraphQL where

import Control.Monad (void)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HashMap
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Scientific (floatingOrInteger)
import Data.Singletons.TH (Sing, SingI, SomeSing(..), fromSing, genSingletons, sing, toSing)
import Data.Singletons.TypeLits (SSymbol)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (typeRep)
import qualified Data.Vector as Vector
import Data.Void (Void)
import Fcf (Eval, Find, FromMaybe, Fst, Snd, TyEq, type (<=<), type (=<<))
import GHC.TypeLits hiding (Text)
import qualified GHC.TypeLits as GHC
import Language.Haskell.TH (DecsQ, ExpQ, appE, appTypeE, lamE, listE, litT, mkName, newName, strTyLit, tupE, varE, varP)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.Megaparsec (Parsec, between, choice, eof, optional, many, parseErrorPretty, runParser, sepBy1, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, space, string)

-------------------------------------------------------------------------------
-- Object stuff
-------------------------------------------------------------------------------

newtype Object schema = Object Aeson.Object
  deriving (Show)

fromObject :: Object schema -> Aeson.Object
fromObject = coerce

-------------------------------------------------------------------------------
-- Schema stuff
-------------------------------------------------------------------------------

-- | A schema for a GraphQL result
data SchemaGraph s
  = SchemaBool
  | SchemaInt
  | SchemaDouble
  | SchemaText
  | SchemaScalar
  | SchemaEnum
  | SchemaMaybe (SchemaGraph s)
  | SchemaList (SchemaGraph s)
  | SchemaObject [(s, SchemaGraph s)]
  deriving (Show)

-- | Kind-level SchemaGraph
type SchemaGraphK = SchemaGraph Symbol
-- | Type-level SchemaGraph
type SchemaGraphT = SchemaGraph Text

$(genSingletons [''SchemaGraph])

-- | A type family converting a SchemaGraph into its return type
type family FromSchema (schema :: SchemaGraphK) where
  FromSchema 'SchemaBool = Bool
  FromSchema 'SchemaInt = Int
  FromSchema 'SchemaDouble = Double
  FromSchema 'SchemaText = Text
  FromSchema 'SchemaScalar = Text -- TODO: include function to convert to scalar
  FromSchema 'SchemaEnum = Text -- TODO: include function to convert to enum
  FromSchema ('SchemaMaybe schema) = Maybe (FromSchema schema)
  FromSchema ('SchemaList schema) = [FromSchema schema]
  FromSchema ('SchemaObject schema) = Object ('SchemaObject schema)

fromSchema :: forall schema. SingI schema => Value -> Either SchemaGraphT (FromSchema schema)
fromSchema = fromSchema' (sing @_ @schema)
  where
    fromSchema' :: forall schema'. SSchemaGraph schema' -> Value -> Either SchemaGraphT (FromSchema schema')
    fromSchema' schema value = case (schema, value) of
      (SSchemaBool, Aeson.Bool b) -> Right b
      (SSchemaInt, Aeson.Number n) | Right i <- floatingOrInteger n -> Right i
      (SSchemaDouble, Aeson.Number n) | Left d <- floatingOrInteger n -> Right d
      (SSchemaText, Aeson.String t) -> Right t
      (SSchemaScalar, Aeson.String t) -> Right t
      (SSchemaEnum, Aeson.String t) -> Right t
      (SSchemaMaybe _, Aeson.Null) -> Right Nothing
      (SSchemaMaybe inner, v) -> Just <$> fromSchema' inner v
      (SSchemaList inner, Aeson.Array a) -> mapM (fromSchema' inner) $ Vector.toList a
      (SSchemaObject _, Aeson.Object o) -> Right $ Object o
      _ -> Left $ fromSing schema

-------------------------------------------------------------------------------
-- Enum stuff
-------------------------------------------------------------------------------

-- class (IsGraphQLEnum e ~ True) => GraphQLEnum e where
--   getEnum :: Proxy e -> String -> e

class result ~ FromSchema 'SchemaEnum => GraphQLEnum enum result where
  getEnum :: SSymbol enum -> String -> result

-------------------------------------------------------------------------------
-- Parse stuff
-------------------------------------------------------------------------------

type family LookupSchema (key :: Symbol) (schema :: SchemaGraphK) :: SchemaGraphK where
  LookupSchema key (SchemaObject schema) = Eval
    ( Snd
    =<< FromMaybe (TypeError
      (    GHC.Text "Key '"
      :<>: GHC.Text key
      :<>: GHC.Text "' does not exist in the following schema:"
      :$$: ShowType schema
      ))
    =<< Find (TyEq key <=< Fst) schema
    )
  LookupSchema key schema = TypeError
    (    GHC.Text "Attempted to lookup key '"
    :<>: GHC.Text key
    :<>: GHC.Text "' in the following schema:"
    :$$: ShowType schema
    )

getKey
  :: forall key schema result
   . (KnownSymbol key, result ~ LookupSchema key schema, SingI result)
  => Object schema
  -> FromSchema result
getKey (Object object) = case fromSchema @result value of
  Right v -> v
  Left schema -> error $ concat
    [ "Could not cast `"
    , show value
    , "` at key '"
    , key
    , "' with schema: "
    , show schema
    ]
  where
    key = symbolVal (Proxy @key)
    value = HashMap.lookupDefault missing (Text.pack key) object
    missing = error $ "Key missing from Object: " ++ key

-------------------------------------------------------------------------------
-- Getter stuff
-------------------------------------------------------------------------------

type Parser = Parsec Void String

parse :: Monad m => Parser a -> String -> m a
parse parser s = either (fail . parseErrorPretty) return $ runParser parser s s

data GetterExp = GetterExp
  { start       :: Maybe String
  , getterOps   :: GetterOps
  } deriving (Show)

getterExp :: Parser GetterExp
getterExp = do
  space
  start <- optional identifier
  getterOps <- many getterOp
  space
  void eof
  return GetterExp{..}

data GetterDecs = GetterDecs
  { startSchema :: String
  , endSchema   :: String
  , getterOps   :: GetterOps
  } deriving (Show)

getterDecs :: Parser GetterDecs
getterDecs = do
  space
  startSchema <- identifier
  space
  string ">"
  space
  getterOps <- many getterOp
  space
  string ">"
  space
  endSchema <- identifier
  space
  void eof
  return GetterDecs{..}

type GetterOps = [GetterOperation]

data GetterOperation
  = GetterKey String
  | GetterKeyList [GetterOps]
  | GetterKeyTuple [GetterOps]
  | GetterBang
  | GetterMap
  deriving (Show)

identifier :: Parser String
identifier = (:) <$> lowerChar <*> many (alphaNumChar <|> char '\'')

getterOp :: Parser GetterOperation
getterOp = choice
  [ string "!" $> GetterBang
  , choice [string "?", string "[]"] $> GetterMap
  , optional (string ".") *> choice
      [ fmap GetterKey identifier
      , fmap GetterKeyList $ between (string "[") (string "]") $ many getterOp `sepBy1` string ","
      , fmap GetterKeyTuple $ between (string "(") (string ")") $ many getterOp `sepBy1` string ","
      ]
  ]

generateGetterExp :: String -> ExpQ
generateGetterExp input = do
  GetterExp{..} <- parse getterExp input
  let apply = appE (mkGetter getterOps) . varE
  case start of
    Nothing -> do
      arg <- newName "x"
      lamE [varP arg] (apply arg)
    Just arg -> apply $ mkName arg
  where
    mkGetter [] = [| id |]
    mkGetter (op:ops) =
      let next = mkGetter ops
      in case op of
        GetterKey key ->
          let getKey' = appTypeE [|getKey|] (litT $ strTyLit key)
          in [| $(next) . $(getKey') |]
        GetterKeyList ops -> do
          val <- newName "v"
          lamE [varP val] (listE $ applyValToOps val ops)
        GetterKeyTuple ops -> do
          val <- newName "v"
          lamE [varP val] (tupE $ applyValToOps val ops)
        GetterBang -> [| $(next) . fromJust |]
        GetterMap -> [| ($(next) <$>) |]
    applyValToOps val ops = map ((`appE` varE val) . mkGetter) ops

generateGetterDecs :: String -> DecsQ
generateGetterDecs input = do
  GetterDecs{..} <- parse getterDecs input
  undefined

-- [get| result.repository.ref!.target |]
--   ==> getKey @"target" . fromJust . getKey @"ref" . getKey @"repository" $ result
get :: QuasiQuoter
get = QuasiQuoter
  { quoteExp = generateGetterExp
  , quoteDec = error "Cannot use `get` for Dec"
  , quoteType = error "Cannot use `get` for Type"
  , quotePat = error "Cannot use `get` for Pat"
  }

-- [getter| Branch.Schema > repository.ref!.target > Branch |]
--   ==> type BranchSchema = ...
--   ==> getBranch :: Object (...) -> Object BranchSchema
getter :: QuasiQuoter
getter = QuasiQuoter
  { quoteExp = error "Cannot use `getter` for Exp"
  , quoteDec = generateGetterDecs
  , quoteType = error "Cannot use `getter` for Type"
  , quotePat = error "Cannot use `getter` for Pat"
  }

-------------------------------------------------------------------------------
-- Query stuff
-------------------------------------------------------------------------------

-- execQuery
--   :: (MonadIO m, IsQueryable result, schema ~ ResultSchema result)
--   => Query schema
--   -> QueryArgs result
--   -> QueryT m (GraphQLResult (Object schema))
-- execQuery = undefined

class IsQueryable result where
  type QueryArgs result = args | args -> result
  type ResultSchema result = (schema :: SchemaGraphK) | schema -> result
  fromArgs :: QueryArgs result -> Value
