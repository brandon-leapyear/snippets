{-# LANGUAGE DataKinds #-}

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
   ]
