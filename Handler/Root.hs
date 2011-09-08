{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Root 
       (
         getRootR
       ) where

import Foundation
import Yesod.Goodies.Markdown

getRootR :: Handler RepHtml
getRootR = do
  articles <- runDB $ selectList [] [Desc ArticleDate, OffsetBy 3]
  defaultLayout $ do
    setTitle "Startseite"
    addWidget $(widgetFile "root")
