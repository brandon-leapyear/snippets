{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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

import Control.Monad (void, (>=>))
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Kind as Kind
import Data.Maybe (fromJust, fromMaybe)
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
import Language.Haskell.TH (Dec(..), DecsQ, ExpQ, Info(..), TyLit(..), Type(..), appE, appT, appTypeE, arrowT, clause, conT, funD, lamE, listE, litT, lookupTypeName, mkName, newName, normalB, reify, runIO, sigD, strTyLit, tupE, tySynD, varE, varP)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.Megaparsec (Parsec, between, choice, eof, optional, many, parseErrorPretty, runParser, sepBy1, some, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, lowerChar, space, string, upperChar)
import Debug.Trace

-------------------------------------------------------------------------------
-- Object stuff
-------------------------------------------------------------------------------

newtype Object (schema :: SchemaGraphK) = Object Aeson.Object
  deriving (Show)

fromObject :: Object schema -> Aeson.Object
fromObject = coerce

-------------------------------------------------------------------------------
-- Enum stuff
-------------------------------------------------------------------------------

class GraphQLEnum e where
  getEnum :: String -> e

type family ToEnum (s :: Symbol) :: Kind.Type

parseValueEnum :: forall e. GraphQLEnum e => Value -> Maybe e
parseValueEnum = \case
  Aeson.String t -> Just $ getEnum @e $ Text.unpack t
  _ -> Nothing

-------------------------------------------------------------------------------
-- Schema stuff
-------------------------------------------------------------------------------

-- | A schema for a GraphQL result
data SchemaGraph s
  = SchemaBool
  | SchemaInt
  | SchemaDouble
  | SchemaText
  | SchemaEnum s
  | SchemaMaybe (SchemaGraph s)
  | SchemaList (SchemaGraph s)
  | SchemaObject [(s, SchemaGraph s)]
  deriving (Show)

-- | Kind-level SchemaGraph
type SchemaGraphK = SchemaGraph Symbol
-- | Type-level SchemaGraph
type SchemaGraphT = SchemaGraph Text

-- | Generates functions that can bring schemas from the type level to the term level.
$(genSingletons [''SchemaGraph])

-- | Constraints to be put on results for a given schema type.
type family ResultConstraints (schema :: SchemaGraphK) result :: Kind.Constraint where
  ResultConstraints ('SchemaEnum _) result = GraphQLEnum result
  ResultConstraints _ _ = ()

-- | A type-class for types that can be parsed from JSON for an associated schema type.
class FromSchema result where
  type ToSchema result = (schema :: SchemaGraphK) | schema -> result
  parseValue :: Value -> Maybe result

instance FromSchema Bool where
  type ToSchema Bool = 'SchemaBool
  parseValue = \case
    Aeson.Bool b -> Just b
    _ -> Nothing

instance FromSchema Int where
  type ToSchema Int = 'SchemaInt
  parseValue = \case
    Aeson.Number n | Right i <- floatingOrInteger n -> Just i
    _ -> Nothing

instance FromSchema Double where
  type ToSchema Double = 'SchemaDouble
  parseValue = \case
    Aeson.Number n | Left i <- floatingOrInteger n -> Just i
    _ -> Nothing

instance FromSchema Text where
  type ToSchema Text = 'SchemaText
  parseValue = \case
    Aeson.String t -> Just t
    _ -> Nothing

instance FromSchema inner => FromSchema (Maybe inner) where
  type ToSchema (Maybe inner) = ('SchemaMaybe (ToSchema inner))
  parseValue = \case
    Aeson.Null -> Just Nothing
    v -> Just <$> parseValue v

instance FromSchema inner => FromSchema [inner] where
  type ToSchema [inner] = ('SchemaList (ToSchema inner))
  parseValue = \case
    Aeson.Array a -> mapM parseValue $ Vector.toList a
    _ -> Nothing

instance FromSchema (Object ('SchemaObject inner)) where
  type ToSchema (Object ('SchemaObject inner)) = 'SchemaObject inner
  parseValue = \case
    Aeson.Object o -> Just $ Object o
    _ -> Nothing

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
  :: forall key schema endSchema result
   . ( KnownSymbol key                     -- for symbolVal
     , SingI schema                        -- to print schema for debugging
     , endSchema ~ LookupSchema key schema -- lookup key in schema for resulting schema
     , ToSchema result ~ endSchema         -- the final result type's associated schema should match resulting schema
     , FromSchema result                   -- ensure result can be converted from a JSON object
     , ResultConstraints endSchema result  -- add any additional constraints on the result type
     )
  => Object schema
  -> result
getKey (Object object) = case parseValue @result value of
  Just v -> v
  Nothing -> error $ concat
    ["Could not cast `", show value, "` at key '", key, "' with schema: ", show schema]
  where
    key = symbolVal (Proxy @key)
    value = HashMap.lookupDefault missing (Text.pack key) object
    missing = error $ "Key missing from Object: " ++ key
    schema = fromSing $ sing @_ @schema

-------------------------------------------------------------------------------
-- Getter stuff
-------------------------------------------------------------------------------

type Parser = Parsec Void String

parse :: Monad m => Parser a -> String -> m a
parse parser s = either (fail . parseErrorPretty) return $ runParser parser s s

type GetterOps = [GetterOperation]

data GetterOperation
  = GetterKey String
  | GetterKeyList [GetterOps]
  | GetterKeyTuple [GetterOps]
  | GetterBang
  | GetterMap
  deriving (Show)

getterOp :: Parser GetterOperation
getterOp = choice
  [ string "!" $> GetterBang
  , choice [string "?", string "[]"] $> GetterMap
  , optional (string ".") *> choice
      [ fmap GetterKey $ identifier lowerChar
      , fmap GetterKeyList $ between (string "[") (string "]") $ some getterOp `sepBy1` string ","
      , fmap GetterKeyTuple $ between (string "(") (string ")") $ some getterOp `sepBy1` string ","
      ]
  ]

identifier :: Parser Char -> Parser String
identifier start = (:) <$> start <*> many (alphaNumChar <|> char '\'')

lexeme :: String -> Parser ()
lexeme s = space >> string s >> space

data GetterExp = GetterExp
  { start       :: Maybe String
  , getterOps   :: GetterOps
  } deriving (Show)

getterExp :: Parser GetterExp
getterExp = do
  space
  start <- optional $ identifier lowerChar
  getterOps <- many getterOp
  space
  void eof
  return GetterExp{..}

generateGetterExp :: GetterExp -> ExpQ
generateGetterExp GetterExp{..} =
  case start of
    Nothing -> do
      arg <- newName "x"
      lamE [varP arg] (apply arg)
    Just arg -> apply $ mkName arg
  where
    apply = appE (mkGetter getterOps) . varE
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

-- [get| result.repository.ref!.target |]
--   ==> getKey @"target" . fromJust . getKey @"ref" . getKey @"repository" $ result
get :: QuasiQuoter
get = QuasiQuoter
  { quoteExp = parse getterExp >=> generateGetterExp
  , quoteDec = error "Cannot use `get` for Dec"
  , quoteType = error "Cannot use `get` for Type"
  , quotePat = error "Cannot use `get` for Pat"
  }

data GetterDecs = GetterDecs
  { startSchema :: String
  , getterOps   :: GetterOps
  , endSchema   :: String
  } deriving (Show)

getterDecs :: Parser GetterDecs
getterDecs = do
  space
  startSchema <- identifier upperChar
  lexeme ">"
  getterOps <- many getterOp
  lexeme ">"
  endSchema <- identifier upperChar
  space
  void eof
  return GetterDecs{..}

generateGetterDecs :: GetterDecs -> DecsQ
generateGetterDecs GetterDecs{..} = do
  getterFuncName <- newName $ "get" ++ endSchema
  endSchemaName <- newName endSchema
  startSchemaName <- maybe (fail $ "Unknown schema: " ++ startSchema) return =<< lookupTypeName startSchema
  startSchema' <- reify startSchemaName >>= \case
    TyConI (TySynD _ _ ty) -> return ty
    info -> fail $ "Unknown type to generate getter function for: " ++ show info
  let getterType = tySynD endSchemaName [] $ fromSchemaType $ fromOps startSchema' getterOps
      getterBody = generateGetterExp $ GetterExp Nothing getterOps
      getterFunc = funD getterFuncName [clause [] (normalB getterBody) []]
  sequence [getterType, getterFunc]
  where
    unSig = \case
      SigT ty _ -> ty
      ty -> ty
    fromOps = foldl getType
    getType schema op = case unSig schema of
      AppT (PromotedT ty) inner ->
        case op of
          GetterKey key | ty == 'SchemaObject ->
            fromMaybe (error $ "Key '" ++ key ++ "' does not exist in schema: " ++ show schema)
            $ lookup key $ getObjectSchema inner
          GetterKey key -> error $ "Cannot get key '" ++ key ++ "' in schema: " ++ show schema
          GetterKeyList elems | ty == 'SchemaObject ->
            let (elemType:rest) = map (fromOps schema) elems
            in if all (== elemType) rest
              then elemType -- return the wrapped type
              else error $ "List contains different types with schema: " ++ show schema
          GetterKeyList _ -> error $ "Cannot get keys in schema: " ++ show schema
          GetterKeyTuple elems | ty == 'SchemaObject ->
            foldl (\acc ops -> AppT acc $ fromOps schema ops) (TupleT $ length elems) elems
          GetterKeyTuple _ -> error $ "Cannot get keys in schema: " ++ show schema
          GetterBang | ty == 'SchemaMaybe -> inner
          GetterBang -> error $ "Cannot use `!` operator on schema: " ++ show schema
          GetterMap | ty == 'SchemaList -> inner -- return the wrapped type
          GetterMap -> error $ "Cannot map over schema: " ++ show schema
      _ -> error $ unlines ["Cannot get type:", show schema, show op]
    getObjectSchema schema = case unSig schema of
      AppT (AppT PromotedConsT t1) t2 ->
        case unSig t1 of
          AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit key))) ty -> (key, ty) : getObjectSchema t2
          _ -> error $ "Could not parse a (key, schema) tuple: " ++ show t1
      PromotedNilT -> []
      t -> error $ "Could not get object schema: " ++ show t
    fromSchemaType schema = case unSig schema of
      AppT (PromotedT ty) inner
        | ty == 'SchemaEnum -> [t| ToEnum $(pure inner) |]
        | ty == 'SchemaMaybe -> [t| Maybe $(fromSchemaType inner) |]
        | ty == 'SchemaList -> [t| [$(fromSchemaType inner)] |]
        | ty == 'SchemaObject -> [t| Object $(pure schema) |]
      PromotedT ty
        | ty == 'SchemaBool -> [t| Bool |]
        | ty == 'SchemaInt -> [t| Int |]
        | ty == 'SchemaDouble -> [t| Double |]
        | ty == 'SchemaText -> [t| Text |]
      AppT t1 t2 -> appT (fromSchemaType t1) (fromSchemaType t2)
      TupleT _ -> pure schema
      _ -> error $ "Could not convert schema: " ++ show schema

-- [getter| Branch.Schema > repository.ref.target > Branch |]
--   ==> type Branch = ...
--   ==> getBranch = ...
getter :: QuasiQuoter
getter = QuasiQuoter
  { quoteExp = error "Cannot use `getter` for Exp"
  , quoteDec = parse getterDecs >=> generateGetterDecs
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
