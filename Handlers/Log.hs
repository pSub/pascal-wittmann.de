{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Log
       ( getLogR
       , getNewLogR
       , postNewLogR
       , getEditLogR
       , postEditLogR
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
  mu <- maybeAuth
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

getEditLogR :: ArticleId -> Handler RepHtml
getEditLogR id = do
  requireAuth
  ma <- runDB $ get id
  case ma of
    Just a ->
      do (_, form, enctype) <- runFormGet $ paramsFormlet $ Just $ Params (articleTitle a) (articleContent a)
         defaultLayout $ do
         $(Settings.hamletFile "newlog")
    Nothing -> do
      redirect RedirectTemporary NewLogR
    
postEditLogR :: ArticleId -> Handler ()
postEditLogR id = do
  (uid, _ ) <- requireAuth
  (res, _, _) <- runFormPostNoNonce $ paramsFormlet Nothing
  case res of
    FormSuccess (Params title text) -> do
      runDB $ update id [ArticleTitle title, ArticleContent text]
      redirect RedirectTemporary LogR
    _ -> do
      redirect RedirectTemporary (EditLogR id)

