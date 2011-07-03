{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Root 
       (
         getRootR
       ) where

import Homepage
import Yesod

getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
  setTitle "Test"
  addHamlet [hamlet|Test|]
