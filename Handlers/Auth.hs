{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Auth
       (
         getAuthR
         ) where

import Homepage
import qualified Settings
import Yesod
import Text.Hamlet
import Text.Cassius

getAuthR :: Handler RepHtml
getAuthR = do 
  mu <- maybeAuth
  defaultLayout $ do
    $(Settings.hamletFile "auth")
