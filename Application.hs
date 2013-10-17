{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import           Control.Monad.Logger                 (runLoggingT)
import qualified Database.Persist
import           Database.Persist.Sql                 (runMigration)
import           Import
import           Network.HTTP.Conduit                 (def, newManager)
import           Network.Wai.Middleware.RequestLogger
import           Settings
import           System.IO                            (stdout)
import           System.Log.FastLogger                (mkLogger)
import           Yesod.Auth
import           Yesod.Default.Config
import           Yesod.Default.Handlers
import           Yesod.Default.Main


-- Import all relevant handler modules here.
import           Handler.Admin
import           Handler.Entries
import           Handler.Impressum
import           Handler.News
import           Handler.Profile
import           Handler.Root
import           Handler.Sitemap
-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = Logger $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    return $ logWare app

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)
    logger <- mkLogger True stdout
    let foundation = App conf s p manager dbconf logger

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }


-- -- This line actually creates our YesodSite instance. It is the second half
-- -- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- -- the comments there for more details.
-- mkYesodDispatch "App" resourcesApp

-- -- This function allocates resources (such as a database connection pool),
-- -- performs initialization and creates a WAI application. This is also the
-- -- place to put your migrate statements to have automatic database
-- -- migrations handled by Yesod.
-- makeApplication :: AppConfig DefaultEnv Extra -> IO Application
-- makeApplication conf = do
--     foundation <- makeFoundation conf
--     app <- toWaiAppPlain foundation
--     return $ logWare app
--   where
--     logWare   = if development then logStdoutDev
--                                else logStdout

-- makeFoundation :: AppConfig DefaultEnv Extra -> IO App
-- makeFoundation conf = do
--     manager <- newManager def
--     s <- staticSite
--     dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
--               Database.Persist.Store.loadConfig >>=
--               Database.Persist.Store.applyEnv
--     p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
--     logger <- mkLogger True stdout
--     let foundation = App conf s p manager dbconf logger

--     -- Perform database migration using our application's logging settings.
--     runLoggingT
--         (Database.Persist.Store.runPool dbconf (runMigration migrateAll) p)
--         (messageLoggerSource foundation logger)

--     return foundation

-- -- for yesod devel
-- getApplicationDev :: IO (Int, Application)
-- getApplicationDev =
--     defaultDevelApp loader makeApplication
--   where
--     loader = loadConfig (configSettings Development)
--         { csParseExtra = parseExtra
--         }


---- This function allocates resources (such as a database connection pool),
---- performs initialization and creates a WAI application. This is also the
---- place to put your migrate statements to have automatic database
---- migrations handled by Yesod.
--getApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
--getApplication conf logger = do
--    manager <- newManager def
--    s <- staticSite
--    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
--              Database.Persist.Store.loadConfig >>=
--              Database.Persist.Store.applyEnv
--    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
--    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
--    let foundation = Homepage conf setLogger s p manager dbconf
--    app <- toWaiAppPlain foundation
--    return $ logWare app
--  where
-- #ifdef DEVELOPMENT
--    logWare = logCallbackDev (logBS setLogger)
--    setLogger = logger
-- #else
--    setLogger = toProduction logger -- by default the logger is set for development
--    logWare = logCallback (logBS setLogger)
-- #endif
--
---- for yesod devel
--getApplicationDev :: IO (Int, Application)
--getApplicationDev =
--    defaultDevelApp loader getApplication
--  where
--    loader = loadConfig (configSettings Development)
--        { csParseExtra = parseExtra
--        }
