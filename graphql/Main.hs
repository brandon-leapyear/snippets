{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap
import GraphQL

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

-- getName :: [get| @branch |] -> Text -- QuasiQuoter should output (Object (SchemaObject ...))
-- getName branch = [get| @branch.name |]

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
                [ asObject [("x", Aeson.Null)]
                , asObject [("x", Aeson.Bool True)]
                , asObject [("x", Aeson.Bool False)]
                ]
              )
            , ("date", Aeson.String "12/25/1970")
            ]
        ) :: Object
          ( SchemaObject
            '[ '("foo", SchemaObject
                  '[ '("bar", SchemaInt)
                   ]
                )
             , '("name", SchemaText)
             , '("xs", SchemaList (SchemaObject (
                  '[ '("x", SchemaMaybe SchemaBool)
                   ])
                ))
             , '("date", SchemaMaybe SchemaScalar)
             ]
          )
  print $ getKey @"foo" result
  print $ fromObject $ getKey @"foo" result
  print $ getKey @"bar" $ getKey @"foo" result
  print $ getKey @"name" result
  print $ getKey @"xs" result
  print $ fmap (getKey @"x") $ getKey @"xs" result
  print $ getKey @"x" .$ getKey @"xs" $ result
  print $ getKey @"date" result
