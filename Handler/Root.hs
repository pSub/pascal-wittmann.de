module Handler.Root where

import           Data.List as L (find)
import           Import
import           ILoveFS

getRootR :: Handler Html
getRootR = do
  posts <- runDB $ select $ from $ \e -> do
                   orderBy [desc (e ^. EntryDate)]
                   limit 5
                   return e
  cats <- runDB $ select $ from $ \c -> do
                  orderBy [asc (c ^. CategoryName)]
                  return c
  defaultLayout $ do
    setTitle "Pascal Wittmann"
    $(widgetFile "root") >> ilovefs
  where findCat p = L.find (\ c -> entityKey c == entryCat p)

nofacebookme_png :: StaticRoute
nofacebookme_png = StaticRoute ["no-facebook-me.png"] []

haskeller_logo :: StaticRoute
haskeller_logo = StaticRoute ["haskeller.png"] []


