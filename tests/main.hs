{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Application          (makeFoundation)
import           Import
import           Test.Hspec           (hspec)
import           Yesod.Default.Config
import           Yesod.Test

import           HomeTest


main = return ()

-- main :: IO ()
-- main = do
--     conf <- Yesod.Default.Config.loadConfig $ (configSettings Testing)
--                 { csParseExtra = parseExtra
--                 }
--     foundation <- makeFoundation conf
--     hspec $ do
--         yesodSpec foundation $ do
--             homeSpecs
