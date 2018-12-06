{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Schema where

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
   , '("date", 'SchemaMaybe 'SchemaText)
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
