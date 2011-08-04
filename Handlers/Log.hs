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
import Data.Text (pack)

data Params = Params
     { title :: Text
     , cat :: Key Category
     , text :: Markdown
     }

getLogR :: Handler RepHtml
getLogR = do
  mu <- maybeAuth
  articles <- runDB $ selectList [] [ArticleDateDesc] 0 0 
  defaultLayout $ do
  setTitle "Log"
  $(Settings.hamletFile "log")
     
paramsFormlet :: Maybe Params -> [(Key Category, Text)] -> Form s m Params
paramsFormlet mparams opts = fieldsToTable $ Params
    <$> stringField "Title" (fmap title mparams)
    <*> selectField opts "Kategorie" (fmap cat mparams)
    <*> markdownField "Text" (fmap text mparams)

getNewLogR :: Handler RepHtml
getNewLogR = do
  requireAuth
  catOpt <- categories
  (_, form, enctype) <- runFormGet $ paramsFormlet Nothing catOpt
  defaultLayout $ do
    $(Settings.hamletFile "newlog")


postNewLogR :: Handler ()
postNewLogR = do
  (uid, _) <- requireAuth
  catOpt <- categories
  (res, _, _) <- runFormPostNoNonce $ paramsFormlet Nothing catOpt
  case res of
    FormSuccess (Params title cat text) -> do
      now <- liftIO getCurrentTime
      runDB $ insert $ Article title text cat "" now
      redirect RedirectTemporary LogR
    _ -> do
      redirect RedirectTemporary NewLogR

getEditLogR :: ArticleId -> Handler RepHtml
getEditLogR id = do
  requireAuth
  ma <- runDB $ get id
  catOpt <- categories
  case ma of
    Just a ->
      do (_, form, enctype) <- runFormGet $ paramsFormlet (Just $ Params (articleTitle a) (articleCat a) (articleContent a)) catOpt
         defaultLayout $ do
         $(Settings.hamletFile "newlog")
    Nothing -> do
      redirect RedirectTemporary NewLogR
    
postEditLogR :: ArticleId -> Handler ()
postEditLogR id = do
  (uid, _ ) <- requireAuth
  catOpt <- categories
  (res, _, _) <- runFormPostNoNonce $ paramsFormlet Nothing catOpt
  case res of
    FormSuccess (Params title cat text) -> do
      runDB $ update id [ArticleTitle title, ArticleCat cat, ArticleContent text]
      redirect RedirectTemporary LogR
    _ -> do
      redirect RedirectTemporary (EditLogR id)

categories = do
  cas <- runDB $ selectList [] [CategoryNameAsc] 0 0
  return $ map (\ c -> (fst c, pack $ categoryName $ snd c)) cas
