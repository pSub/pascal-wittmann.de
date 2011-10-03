{-# LANGUAGE CPP, DeriveDataTypeable #-}
import Settings (AppEnvironment(..), AppConfig(..), loadConfig)
import Application (withHomepage)
import Network.Wai.Handler.Warp (run)
import System.Console.CmdArgs hiding (args)
import Data.Char (toUpper, toLower)

#ifndef PRODUCTION
import Network.Wai.Middleware.Debug (debugHandle)
import Yesod.Logger (logString, logLazyText, flushLogger, makeLogger)
#else
import Yesod.Logger (makeLogger)
#endif

main :: IO ()
main = do
    logger <- makeLogger
    args   <- cmdArgs argConfig
    env    <- getAppEnv args
    config <- loadConfig env
    let c = if port args /= 0
            then config { appPort = port args }
            else config

#if PRODUCTION
    withHomepage c logger $ run (appPort c)
#else
    logString logger $ (show env) ++ " application launched, listening on port " ++ show (appPort c)
    withHomepage c logger $ run (appPort c) . debugHandle (logHandle logger)
    flushLogger logger

    where
        logHandle logger msg = logLazyText logger msg >> flushLogger logger
#endif

data ArgConfig = ArgConfig
    { environment :: String
    , port        :: Int
    } deriving (Show, Data, Typeable)

argConfig :: ArgConfig
argConfig = ArgConfig
    { environment = def 
        &= help ("application environment, one of: " ++ (foldl1 (\a b -> a ++ ", " ++ b) environments))
        &= typ "ENVIRONMENT"
    , port = def
        &= typ "PORT"
    }

environments :: [String]
environments = map ((map toLower) . show) ([minBound..maxBound] :: [AppEnvironment])

-- | retrieve the -e environment option
getAppEnv :: ArgConfig -> IO AppEnvironment
getAppEnv cfg = do
    let e = if environment cfg /= ""
            then environment cfg
            else
#if PRODUCTION
                "production"
#else
                "development"
#endif
    return $ read $ capitalize e

    where
        capitalize [] = []
        capitalize (x:xs) = toUpper x : map toLower xs