{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Linux
       (
         getLinuxR
         ) where

import Homepage
import qualified Settings
import Yesod

getLinuxR :: Handler RepHtml
getLinuxR = defaultLayout $ do
  setTitle "Linux"
  $(Settings.hamletFile "linux")
