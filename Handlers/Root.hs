{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Root 
       (
         getRootR
       ) where

import Homepage
import Model
import qualified Settings
import Yesod
import Yesod.Goodies.Markdown

getRootR :: Handler RepHtml
getRootR = do
  articles <- runDB $ selectList [] [ArticleDateDesc] 3 0
  defaultLayout $ do
    setTitle "Startseite"
    $(Settings.hamletFile "root")
