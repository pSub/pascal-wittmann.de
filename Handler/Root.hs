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
  defaultLayout $ do
    setTitle "Startseite"
    addWidget $(widgetFile "root")

nofacebookme_png :: StaticRoute
nofacebookme_png = StaticRoute ["no-facebook-me.png"] []
