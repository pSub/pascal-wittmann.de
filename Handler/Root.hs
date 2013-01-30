module Handler.Root where

import           Data.List (find)
import           Import

getRootR :: Handler RepHtml
getRootR = do
  posts <- runDB $ selectList [] [Desc EntryDate, LimitTo 5]
  cats <- runDB $ selectList [] [Asc CategoryName]
  defaultLayout $ do
    setTitle "Pascal Wittmann"
    $(widgetFile "root")
  where findCat p cs = find (\ c -> entityKey c == entryCat p) cs

nofacebookme_png :: StaticRoute
nofacebookme_png = StaticRoute ["no-facebook-me.png"] []

haskeller_logo :: StaticRoute
haskeller_logo = StaticRoute ["haskeller.png"] []
