{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Log
       ( getLogR
       , getNewLogR
       , postNewLogR
       ) where

import Homepage
import qualified Settings
import Model
import Yesod
import Yesod.Auth
import Yesod.Goodies.Markdown

import Control.Applicative
import Data.Text (Text)
import Data.Time

data Params = Params
     { title :: Text
     , text :: Markdown
     }

getLogR :: Handler RepHtml
getLogR = do 
  articles <- runDB $ selectList [] [ArticleDateDesc] 0 0 
  defaultLayout $ do
  setTitle "Log"
  $(Settings.hamletFile "log")
     
paramsFormlet :: Maybe Params -> Form s m Params
paramsFormlet mparams = fieldsToTable $ Params
    <$> stringField "Title" (fmap title mparams)
    <*> markdownField "Text" (fmap text mparams)

getNewLogR :: Handler RepHtml
getNewLogR = do
  requireAuth
  (_, form, enctype) <- runFormGet $ paramsFormlet Nothing
  defaultLayout $ do
    $(Settings.hamletFile "newlog")


postNewLogR :: Handler ()
postNewLogR = do
  (uid, _) <- requireAuth
  (res, _, _) <- runFormPostNoNonce $ paramsFormlet Nothing
  case res of
    FormSuccess (Params title text) -> do
      now <- liftIO getCurrentTime
      runDB $ insert $ Article title text "" now
      redirect RedirectTemporary LogR
      return ()
    _ -> do
      redirect RedirectTemporary NewLogR
      return ()
