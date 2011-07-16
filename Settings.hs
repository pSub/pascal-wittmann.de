{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings
       ( approot
       , hamletFile
       , cassiusFile
       , connStr
       , ConnectionPool
       , withConnectionPool
       , runConnectionPool
       ) where

import qualified Text.Hamlet
import qualified Text.Cassius
import qualified Data.Text as T
import Database.Persist.Postgresql
import Yesod (MonadControlIO)

approot :: T.Text
approot = "http://localhost:3000"

hamletFile x  = Text.Hamlet.hamletFile  $ "hamlet/"  ++ x ++ ".hamlet"
cassiusFile x = Text.Cassius.cassiusFile $ "cassius/" ++ x ++ ".cassius"


-- Also, connections are returned to the pool as quickly as possible by
-- Yesod to avoid resource exhaustion. A connection is only considered in
-- use while within a call to runDB.
connectionCount :: Int
connectionCount = 100

connStr = "user=homepage password=homepage host=localhost port=5432 dbname=homepage"

-- The next two functions are for allocating a connection pool and running
-- database actions using a pool, respectively. It is used internally
-- by the scaffolded application, and therefore you will rarely need to use
-- them yourself.
withConnectionPool :: MonadControlIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withPostgresqlPool connStr connectionCount

runConnectionPool :: MonadControlIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool
