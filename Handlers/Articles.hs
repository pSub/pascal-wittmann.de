{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Articles
       ( getArticlesR
       , getNewArticleR
       , postNewArticleR
       , getEditArticleR
       , postEditArticleR
       ) where

import Debug.Trace

import Homepage
import qualified Settings
import Model
import Yesod
import Yesod.Auth
import Yesod.Goodies.Markdown

import Control.Applicative
import Data.List (find, intersperse)
import Data.Time
import Data.Text (Text, pack, unpack)
import Data.Maybe

data Params = Params
     { title :: Text
     , cat :: CategoryId
     , tag :: Text
     , text :: Markdown
     }

getArticlesR :: Text -> Handler RepHtml
getArticlesR cat = do
  mu <- maybeAuth
  mcat <- runDB $ getBy $ CategoryUniq cat
  articles <- runDB $ selectList [ArticleCatEq $ fst $ fromJust mcat] [ArticleDateDesc] 0 0
  tags <- runDB $ selectList [] [TagNameAsc] 0 0
  defaultLayout $ do
  setTitle "Log"
  $(Settings.hamletFile "articles")
     
paramsFormlet :: Maybe Params -> [(CategoryId, Text)] -> Form s m Params
paramsFormlet mparams cats = fieldsToTable $ Params
    <$> stringField "Title" (fmap title mparams)
    <*> selectField cats "Kategorie" (fmap cat mparams)
    <*> stringField "Tags" (fmap tag mparams)
    <*> markdownField "Text" (fmap text mparams)

getNewArticleR :: Handler RepHtml
getNewArticleR = do
  requireAuth
  catOpt <- categories
  (_, form, enctype) <- runFormGet $ paramsFormlet Nothing catOpt
  defaultLayout $ do
    $(Settings.hamletFile "new-article")


postNewArticleR :: Handler ()
postNewArticleR = do
  (uid, _) <- requireAuth
  catOpt <- categories
  (res, _, _) <- runFormPostNoNonce $ paramsFormlet Nothing catOpt
  case res of
    FormSuccess (Params title cat tag text) -> do
      now <- liftIO getCurrentTime
      id <- runDB $ insert $ Article title text cat "" now
      runDB $ insert $ Tag tag id
      redirect RedirectTemporary $ ArticlesR $ category cat catOpt
    _ -> do
      redirect RedirectTemporary NewArticleR

getEditArticleR :: ArticleId -> Handler RepHtml
getEditArticleR id = do
  requireAuth
  ma <- runDB $ get id
  catOpt <- categories
  case ma of
    Just a ->
      do (_, form, enctype) <- runFormGet $ paramsFormlet (Just $ Params (articleTitle a) (articleCat a) "" (articleContent a)) catOpt
         defaultLayout $ do
         $(Settings.hamletFile "new-article")
    Nothing -> do
      redirect RedirectTemporary NewArticleR
    
postEditArticleR :: ArticleId -> Handler ()
postEditArticleR id = do
  (uid, _ ) <- requireAuth
  catOpt <- categories
  (res, _, _) <- runFormPostNoNonce $ paramsFormlet Nothing catOpt
  case res of
    FormSuccess (Params title cat tag text) -> do
      runDB $ update id [ArticleTitle title, ArticleCat cat, ArticleContent text]
      runDB $ insert $ Tag tag id
      redirect RedirectTemporary $ ArticlesR $ category cat catOpt
    _ -> do
      redirect RedirectTemporary (EditArticleR id)

-- Helper functions
categories = do
  cas <- runDB $ selectList [] [CategoryNameAsc] 0 0
  return $ map (\ c -> (fst c, categoryName $ snd c)) cas
  
category cat = snd . fromJust . find ((== cat) . fst)

tagsForArticle id = (intersperse ", ") . map (tagName . snd) . filter ((== id) . tagArticle . snd)
