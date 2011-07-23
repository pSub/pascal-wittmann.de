{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Root 
       (
         getRootR
       ) where

import Homepage
import qualified Settings
import Yesod
import Yesod.Auth
import Text.Hamlet
import Text.Cassius

getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
           setTitle "Startseite"
           $(Settings.hamletFile "homepage")
