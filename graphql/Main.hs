{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap
import GraphQL
import Schema

-------------------------------------------------------------------------------
-- Auto-generated stuff
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
                      , '("state", SchemaEnum)
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

-------------------------------------------------------------------------------
-- Enum stuff
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Test main code
-------------------------------------------------------------------------------

[getter| MySchema > quux! > Quux |]
[getter| MySchema > .[foo.bar,baz] > FooBarBaz |]
[getter| MySchema > .(foo.bar,date) > FooBarDate |]

parseAsdf :: Quux -> Int
parseAsdf = [get| .asdf |]

main :: IO ()
main = do
  let asObject = Aeson.Object . HashMap.fromList
      result = Object
        ( HashMap.fromList
            [ ("foo", asObject
                [ ("bar", Aeson.Number 1)
                ]
              )
            , ("baz", Aeson.Number 2)
            , ("quux", asObject
                [ ("asdf", Aeson.Number 3)
                ]
              )
            , ("name", Aeson.String "foobar")
            , ("xs", Aeson.toJSON
                [ asObject [("x", Aeson.Null)]
                , asObject [("x", Aeson.Bool True)]
                , asObject [("x", Aeson.Bool False)]
                ]
              )
            , ("date", Aeson.String "12/25/1970")
            ]
        ) :: Object MySchema

  putStrLn ">>> Gets"
  print [get| result.foo |]
  print $ fromObject [get| result.foo |]
  print [get| result.foo.bar |]
  print [get| result.name |]
  print [get| result.xs |]
  print [get| result.xs[].x |]
  print [get| result.date |]
  print [get| result.date! |]
  print [get| result.quux?.asdf |]
  print [get| result.[foo.bar,baz] |]
  print [get| result.(foo.bar,name) |]
  let (bar, baz, xs) = [get| result.(foo.bar,baz,xs) |]
  print (bar, baz)
  print $ map [get| .x |] xs

  putStrLn ">>> Getters"
  print $ parseAsdf $ getQuux result
  print $ getFooBarBaz result
  print $ getFooBarDate result
