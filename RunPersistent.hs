{-
  stack runghc
    --package leapyear-persistent-setup
    --package persistent
    --package persistent-sqlite
-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Text (Text)
import Database.Persist.Sql (rawSql)
import Database.Persist.Sqlite (withSqliteConn)

import LeapYear.Persistent.Monad.Persist (SqlQuery, unSqlQuery)

main = runSqlite "db.sqlite" $ rawSql "SELECT * FROM foo"

runSqlite :: Text -> SqlQuery a -> IO a
runSqlite db = runStderrLoggingT . withSqliteConn db . runReaderT . unSqlQuery
