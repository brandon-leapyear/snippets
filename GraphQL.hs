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
  FromSchema ('SchemaEnum _) = Text -- TODO: include function to convert to enum
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
      (SSchemaEnum typ, Aeson.String t) -> Right t
      (SSchemaMaybe _, Aeson.Null) -> Right Nothing
      (SSchemaMaybe inner, v) -> Just <$> fromSchema' inner v
      (SSchemaList inner, Aeson.Array a) -> mapM (fromSchema' inner) $ Vector.toList a
      (SSchemaObject _, Aeson.Object o) -> Right $ Object o
      _ -> Left $ fromSing schema

-- enum stuff

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

{-# INLINE (.$) #-}
(.$) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
f .$ g = fmap f . g
infixr 9 .$

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

-- instance GraphQLEnum StatusState where
--   getEnum _ s = case s of
--     "EXPECTED" -> EXPECTED
--     "ERROR" -> ERROR
--     "FAILURE" -> FAILURE
--     "PENDING" -> PENDING
--     "SUCCESS" -> SUCCESS
--     _ -> error $ "Invalid StatusState: " ++ s

main :: IO ()
main = do
  let asObject = Aeson.Object . HashMap.fromList
      result = Object
        ( HashMap.fromList
            [ ("foo", asObject
                [ ("bar", Aeson.Number 1)
                ]
              )
            , ("name", Aeson.String "foobar")
            , ("xs", Aeson.toJSON
                [ asObject [("x", Aeson.Bool True)]
                , asObject [("x", Aeson.Bool True)]
                , asObject [("x", Aeson.Bool False)]
                ]
              )
            ]
        ) :: Object
          ( SchemaObject
            '[ '("foo", SchemaObject
                  '[ '("bar", SchemaInt)
                   ]
                )
             , '("name", SchemaText)
             , '("xs", SchemaList (SchemaObject (
                  '[ '("x", SchemaBool)
                   ])
                ))
             ]
          )
  print $ getKey @"foo" result
  print $ fromObject $ getKey @"foo" result
  print $ getKey @"bar" $ getKey @"foo" result
  print $ getKey @"name" result
  print $ getKey @"xs" result
  print $ fmap (getKey @"x") $ getKey @"xs" result
  print $ getKey @"x" .$ getKey @"xs" $ result
