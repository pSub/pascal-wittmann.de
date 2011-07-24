{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.NewLog
       (
         getNewLogR
         ) where

import Homepage
import qualified Settings
import Yesod
import Yesod.Auth
import Control.Applicative
import Text.Hamlet
import Text.Cassius
import Data.Text (Text)

data Params = Params
     { title :: Text
     , text :: Text
     }
     
paramsFormlet :: Maybe Params -> Form s m Params
paramsFormlet mparams = fieldsToTable $ Params
    <$> stringField "Title" (fmap title mparams)
    <*> stringField "Text" (fmap text mparams)

getNewLogR :: Handler RepHtml
getNewLogR = do
  requireAuth
  (res, form, enctype) <- runFormGet $ paramsFormlet Nothing
  defaultLayout $ do
    $(Settings.hamletFile "newlog")
