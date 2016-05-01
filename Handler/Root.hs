module Handler.Root where

import           Data.List as L (find)
import           Import
import           Data.Time.LocalTime

getRootR :: Handler Html
getRootR = do
  posts <- runDB $ select $ from $ \e -> do
                   orderBy [desc (e ^. EntryDate)]
                   limit 5
                   return e
  cats <- runDB $ select $ from $ \c -> do
                  orderBy [asc (c ^. CategoryName)]
                  return c
  zonedTime <- liftIO getZonedTime
  defaultLayout $ do
    setTitle "Pascal Wittmann"
    $(widgetFile "root")
  where findCat p cs = L.find (\ c -> entityKey c == entryCat p) cs

isValentinesDay :: ZonedTime -> Bool
isValentinesDay (ZonedTime (LocalTime localDay _) _) =
  let (_, month, day) = toGregorian localDay
      in month == 2 && day == 14

nofacebookme_png :: StaticRoute
nofacebookme_png = StaticRoute ["no-facebook-me.png"] []

haskeller_logo :: StaticRoute
haskeller_logo = StaticRoute ["haskeller.png"] []

ilovefs :: StaticRoute
ilovefs = StaticRoute ["ilovefs-banner-extralarge.png"] []
