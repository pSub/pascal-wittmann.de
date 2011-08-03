{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Log
       (
         getLogR
         ) where

import Homepage
import qualified Settings
import Model
import Yesod
import Yesod.Goodies.Markdown

getLogR :: Handler RepHtml
getLogR = do 
  articles <- runDB $ selectList [] [ArticleDateDesc] 0 0 
  defaultLayout $ do
  setTitle "Log"
  $(Settings.hamletFile "log")
