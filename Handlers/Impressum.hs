{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Impressum
       (
         getImpressumR
         ) where

import Homepage
import qualified Settings
import Yesod
import Text.Hamlet
import Text.Cassius

getImpressumR :: Handler RepHtml
getImpressumR = defaultLayout $ do
  setTitle "Impressum"
  $(Settings.hamletFile "impressum")
  
-- TODO: Protect mail from spam
encodeMail mail = show mail
