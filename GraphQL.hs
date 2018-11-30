{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
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
import Fcf
import GHC.TypeLits hiding (Text)
import qualified GHC.TypeLits as GHC

-------------------------------------------------------------------------------
-- Library code
-------------------------------------------------------------------------------

-- object stuff

newtype Object schema = Object Aeson.Object
  deriving (Show)

fromObject :: Object schema -> Aeson.Object
fromObject = coerce

-- schema stuff

-- | A schema for a GraphQL result
data SchemaGraph s
  = SchemaBool
  | SchemaInt
  | SchemaDouble
  | SchemaText
  | SchemaScalar
  | SchemaEnum s
  | SchemaMaybe (SchemaGraph s)
  | SchemaList (SchemaGraph s)
  | SchemaObject [(s, SchemaGraph s)]
  deriving (Show)

$(genSingletons [''SchemaGraph])

-- | Kind-level SchemaGraph
type SchemaGraphK = SchemaGraph Symbol
-- | Type-level SchemaGraph
type SchemaGraphT = SchemaGraph Text

-- | A type family converting a SchemaGraph into its return type
type family FromSchema (schema :: SchemaGraphK)
type instance FromSchema 'SchemaBool = Bool
type instance FromSchema 'SchemaInt = Int
type instance FromSchema 'SchemaDouble = Double
type instance FromSchema 'SchemaText = Text
type instance FromSchema 'SchemaScalar = Text
type instance FromSchema ('SchemaMaybe schema) = Maybe (FromSchema schema)
type instance FromSchema ('SchemaList schema) = [FromSchema schema]
type instance FromSchema ('SchemaObject schema) = Object ('SchemaObject schema)

-- data FromSchema (schema :: SchemaGraphK) where
--   FromSchemaBool :: Bool -> FromSchema 'SchemaBool
--   FromSchemaInt :: Int -> FromSchema 'SchemaInt
--   FromSchemaDouble :: Double -> FromSchema 'SchemaDouble
--   FromSchemaText :: Text -> FromSchema 'SchemaText
--   FromSchemaScalar :: Text -> FromSchema 'SchemaScalar
--   FromSchemaEnum :: String -> FromSchema ('SchemaEnum keys)
--   FromSchemaMaybe :: Maybe (FromSchema schema) -> FromSchema ('SchemaMaybe schema)
--   FromSchemaList :: [FromSchema schema] -> FromSchema ('SchemaList schema)
--   FromSchemaObject :: Object ('SchemaObject schema) -> FromSchema ('SchemaObject schema)

-- class FromTagged (cons :: [Symbol]) e where
--   fromTagged :: String -> e

-- class SingI (schema :: SchemaGraphK) => FromSchemaC schema x where
--   castFromSchemaC :: FromSchema schema -> x
-- instance FromSchemaC 'FromSchemaBool Bool where
--   castFromSchemaC (FromSchemaBool b) = b
-- instance FromSchemaC 'FromSchemaInt Int where
--   castFromSchemaC (FromSchemaInt i) = i
-- instance FromSchemaC 'FromSchemaDouble Double where
--   castFromSchemaC (FromSchemaDouble d) = d
-- instance FromSchemaC 'FromSchemaText Text where
--   castFromSchemaC (FromSchemaText t) = t
-- instance FromSchemaC 'FromSchemaScalar Text where
--   castFromSchemaC (FromSchemaScalar t) = t
-- instance FromTagged cons e => FromSchemaC ('FromSchemaEnum cons) e where
--   castFromSchemaC (FromSchemaEnum tagged) = fromTagged tagged
-- instance FromSchemaC schema x => FromSchemaC ('FromSchemaMaybe schema) (Maybe x) where
--   castFromSchemaC (FromSchemaMaybe m) = castFromSchemaC <$> m
-- instance FromSchemaC schema x => FromSchemaC ('FromSchemaList schema) [x] where
--   castFromSchemaC (FromSchemaList l) = castFromSchemaC <$> l
-- instance FromSchemaC schema x => FromSchemaC ('FromSchemaObject schema) (Object ('SchemaObject schema)) where
--   castFromSchemaC (FromSchemaObject o) = o

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
      -- (SSchemaEnum typ, Aeson.String t) -> Right $ getEnum typ $ Text.unpack t
      (SSchemaMaybe _, Aeson.Null) -> Right Nothing
      (SSchemaMaybe inner, v) -> Just <$> fromSchema' inner v
      (SSchemaList inner, Aeson.Array a) -> mapM (fromSchema' inner) $ Vector.toList a
      (SSchemaObject _, Aeson.Object o) -> Right $ Object o
      _ -> Left $ fromSing schema

-- enum stuff

-- type family IsGraphQLEnum e :: Bool

-- class (IsGraphQLEnum e ~ True) => GraphQLEnum e where
--   getEnum :: Proxy e -> String -> e

class result ~ FromSchema ('SchemaEnum enum) => GraphQLEnum enum result where
  getEnum :: SSymbol enum -> String -> result

-- parse stuff

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
   -- . (KnownSymbol key, result ~ LookupSchema key schema, FromSchemaC result x)
   . (KnownSymbol key, result ~ LookupSchema key schema, SingI result)
  => Object schema
  -- -> x
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

-- query stuff

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

-------------------------------------------------------------------------------
-- Client code
-------------------------------------------------------------------------------

data Args = Args
  { _repoOwner :: String
  , _repoName  :: String
  , _name      :: String
  }

data Result

type Schema = 'SchemaObject
  '[ '( "repository"
    , 'SchemaObject
      '[ '( "ref"
        , 'SchemaMaybe ('SchemaObject
          '[ '( "target"
            , 'SchemaObject
              '[ '("oid", 'SchemaScalar)
              , '("message", 'SchemaMaybe 'SchemaText)
              , '( "tree"
                , 'SchemaMaybe ('SchemaObject
                  '[ '("oid", 'SchemaScalar)
                  , '("entries", 'SchemaMaybe ('SchemaList ('SchemaObject
                      '[ '("name", 'SchemaText)
                      , '("object", 'SchemaMaybe ('SchemaObject
                          '[ '("text", 'SchemaMaybe 'SchemaText)
                          ])
                        )
                      ]))
                    )
                  ])
                )
              , '( "status"
                , 'SchemaMaybe ('SchemaObject
                  '[ '( "contexts"
                    , 'SchemaList ('SchemaObject
                      '[ '("context", 'SchemaText)
                      -- , '("state", 'SchemaEnum StatusState)
                      , '("state", SchemaEnum "StatusState")
                      ])
                    )
                  ])
                )
              ]
            )
          ])
        )
      ]
    )
  ]

instance IsQueryable Result where
  type QueryArgs Result = Args
  type ResultSchema Result = Schema
  fromArgs args = object
    [ "repoOwner" .= _repoOwner args
    , "repoName"  .= _repoName args
    , "name"      .= _name args
    ]

-- getName :: [get| @branch |] -> Text -- QuasiQuoter should output (Object (SchemaObject ...))
-- getName branch = [get| @branch.name |]

-- enum stuff

data StatusState
  = EXPECTED
  | ERROR
  | FAILURE
  | PENDING
  | SUCCESS
  deriving (Show,Eq,Enum)

-- type instance IsGraphQLEnum StatusState = True

-- instance GraphQLEnum StatusState where
--   getEnum _ s = case s of
--     "EXPECTED" -> EXPECTED
--     "ERROR" -> ERROR
--     "FAILURE" -> FAILURE
--     "PENDING" -> PENDING
--     "SUCCESS" -> SUCCESS
--     _ -> error $ "Invalid StatusState: " ++ s

type instance FromSchema ('SchemaEnum "StatusState") = StatusState

instance GraphQLEnum "StatusState" StatusState where
  getEnum _ s = case s of
    "EXPECTED" -> EXPECTED
    "ERROR" -> ERROR
    "FAILURE" -> FAILURE
    "PENDING" -> PENDING
    "SUCCESS" -> SUCCESS
    _ -> error $ "Invalid StatusState: " ++ s
