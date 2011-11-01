module Handler.Impressum
       (
         getImpressumR
         ) where

import Foundation

getImpressumR :: Handler RepHtml
getImpressumR = defaultLayout $ do
  setTitle "Impressum"
  $(widgetFile "impressum")
  
-- TODO: Protect mail from spam
encodeMail :: String -> Html
encodeMail = toHtml
