module Handler.StaticPages
       (
         getPrivacyR
       , getContactR
       , getAboutR
         ) where

import           Import

getPrivacyR :: Handler Html
getPrivacyR = defaultLayout $ do
  setTitle "Privacy"
  $(widgetFile "privacy")

getContactR :: Handler Html
getContactR = defaultLayout $ do
    setTitle "Contact Me"
    $(widgetFile "contact")

getAboutR :: Handler Html
getAboutR = defaultLayout $ do
    setTitle "About Me"
    $(widgetFile "about")