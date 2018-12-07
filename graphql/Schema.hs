{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Schema where

import Data.Time (ParseTime, UTCTime, defaultTimeLocale, iso8601DateFormat, parseTimeOrError)
import GraphQL

-- | Need to put this here so that it's in type-scope for the getter QuasiQuoter.
type MySchema = 'SchemaObject
  '[ '("foo", 'SchemaObject
        '[ '("bar", 'SchemaInt)
         ]
      )
   , '("baz", 'SchemaInt)
   , '("quux", 'SchemaMaybe ('SchemaObject
        '[ '("asdf", 'SchemaInt)
         ]
      ))
   , '("name", 'SchemaText)
   , '("xs", 'SchemaList ('SchemaObject (
        '[ '("x", 'SchemaMaybe 'SchemaBool)
         ])
      ))
   , '("date", 'SchemaMaybe ('SchemaScalar "MyDate"))
   , '("state", 'SchemaEnum "MyState")
   ]

data MyState = OPEN | CLOSED deriving (Show)

instance GraphQLEnum MyState where
  getEnum s = case s of
    "OPEN" -> OPEN
    "CLOSED" -> CLOSED
    _ -> error $ "Bad MyState: " ++ s

type instance ToEnum "MyState" = MyState

instance FromSchema MyState where
  type ToSchema MyState = 'SchemaEnum "MyState"
  parseValue = parseValueEnum

newtype MyDate = MyDate UTCTime deriving (ParseTime, Show)

instance GraphQLScalar MyDate where
  getScalar = parseTimeOrError False defaultTimeLocale $ iso8601DateFormat Nothing

type instance ToScalar "MyDate" = MyDate

instance FromSchema MyDate where
  type ToSchema MyDate = 'SchemaScalar "MyDate"
  parseValue = parseValueScalar
