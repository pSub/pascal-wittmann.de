module Handler.Impressum
       (
         getImpressumR,
         getDatenschutzR
         ) where

import           Import

getImpressumR :: Handler Html
getImpressumR = defaultLayout $ do
  setTitle "Impressum"
  $(widgetFile "impressum")

-- TODO: Protect mail from spam
encodeMail :: String -> Html
encodeMail = toHtml

getDatenschutzR :: Handler Html
getDatenschutzR = defaultLayout $ do
  setTitle "Datenschutzerklärung"
  $(widgetFile "datenschutzerklaerung")

