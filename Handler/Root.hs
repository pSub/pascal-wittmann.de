tat
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Root 
       (
         getRootR
       ) where

import Foundation
import Data.List (find)

getRootR :: Handler RepHtml
getRootR = do
  posts <- runDB $ selectList [] [Desc EntryDate, LimitTo 5]
  cats <- runDB $ selectList [] [Asc CategoryName]
  defaultLayout $ do
    setTitle "Startseite"
    $(widgetFile "root")
  where findCat p cs = find (\ c -> fst c == entryCat p) cs

nofacebookme_png :: StaticRoute
nofacebookme_png = StaticRoute ["no-facebook-me.png"] []
