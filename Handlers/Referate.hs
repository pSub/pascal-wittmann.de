{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Referate
       (
         getReferateR
         ) where

import Homepage
import qualified Settings
import Yesod

getReferateR :: Handler RepHtml
getReferateR = defaultLayout $ do
  setTitle "Referate"
  $(Settings.hamletFile "referate")
