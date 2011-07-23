{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Controller where

import Yesod
import Yesod.Auth
import Homepage
import Model
import qualified Settings
import Handlers
import Network.Wai.Handler.Warp (run)
import Database.Persist.GenericSql
import Data.Time.Clock

mkYesodDispatch "Homepage" resourcesHomepage

start :: IO ()
start = withHomepage $ run 3000


-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withHomepage :: (Application -> IO a) -> IO a
withHomepage f = Settings.withConnectionPool $ \p -> do
    flip Settings.runConnectionPool p $ runMigration migrateAll
    let h = Homepage p
    toWaiApp h >>= f
