{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Homepage where

import Yesod
import qualified Settings

data Homepage = Homepage

type Handler = GHandler Homepage Homepage
type Widget = GWidget Homepage Homepage

mkYesodData "Homepage" [parseRoutes|
                    / RootR GET
                    /robots.txt RobotsR GET
                   |]

instance Yesod Homepage where
  approot _ =  Settings.approot
