module ILoveFS where

import           Import
import           Data.Time.LocalTime
       
class YesodILoveFS master where
      
ilovefs :: (MonadWidget m, HandlerSite m ~ App) => m ()
ilovefs = do
  zonedTime <- liftIO getZonedTime

  toWidget
      [hamlet|
          $if isValentinesDay zonedTime
            <div style="text-align: center">
              <a href="http://ilovefs.org"><img src=@{StaticR ilovefsRoute} style="border: 0 !important;" alt="I love Free Software!"></a>
      |]

isValentinesDay :: ZonedTime -> Bool
isValentinesDay (ZonedTime (LocalTime localDay _) _) =
  let (_, month, day) = toGregorian localDay
      in month == 2 && day == 14

ilovefsRoute :: StaticRoute
ilovefsRoute = StaticRoute ["ilovefs-banner-extralarge.png"] []
