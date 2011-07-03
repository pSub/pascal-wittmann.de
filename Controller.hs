{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Controller where

import Yesod
import Homepage
import Handlers

mkYesodDispatch "Homepage" resourcesHomepage

start :: IO ()
start = warpDebug 3000 Homepage
