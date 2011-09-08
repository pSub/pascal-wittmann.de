{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Articles
       ( getArticlesR
       , getNewArticleR
       , postNewArticleR
       , getEditArticleR
       , postEditArticleR
       ) where

import Foundation
import Yesod.Goodies.Markdown

import Control.Applicative
import Data.List (find, intersperse)
import Data.Time
import Data.Text (Text)
import Data.Maybe

data Params = Params
     { title :: Text
     , cat :: CategoryId
     , tag :: Text
     , text :: Markdown
     } deriving Show
     
paramsFormlet ::  Maybe Params -> [(Text, CategoryId)] -> Html -> Form Homepage Homepage (FormResult Params, Widget)
paramsFormlet mparams cats html = (flip renderDivs) html $ Params
    <$> areq textField "Title" (title <$> mparams)
    <*> areq (selectField cats) "Kategorie" (cat <$> mparams)
    <*> areq textField "Tags" (tag <$> mparams)
    <*> areq markdownField "Text" (text <$> mparams)

getArticlesR :: Text -> Handler RepHtml
getArticlesR cat = do
  mu <- maybeAuth
  mcat <- runDB $ getBy $ CategoryUniq cat
  articles <- runDB $ selectList [ArticleCat ==. (fst $ fromJust mcat) ] [Desc ArticleDate]
  tags <- runDB $ selectList [] [Asc TagName]
  defaultLayout $ do
  setTitle "Log"
  addWidget $(widgetFile "articles")

getNewArticleR :: Handler RepHtml
getNewArticleR = do
  requireAuth
  catOpt <- categories
  ((_, form), enctype) <- runFormGet $ paramsFormlet Nothing catOpt
  defaultLayout $ do
    addWidget $(widgetFile "new-article")


postNewArticleR :: Handler ()
postNewArticleR = do
  (uid, _) <- requireAuth
  catOpt <- categories
  ((res, _), _) <- runFormPostNoNonce $ paramsFormlet Nothing catOpt
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
      do ((_, form), enctype) <- runFormGet $ paramsFormlet (Just $ Params (articleTitle a) (articleCat a) "" (articleContent a)) catOpt
         defaultLayout $ do
         addWidget $(widgetFile "new-article")
    Nothing -> do
      redirect RedirectTemporary NewArticleR
    
postEditArticleR :: ArticleId -> Handler ()
postEditArticleR id = do
  (uid, _ ) <- requireAuth
  catOpt <- categories
  ((res, _), _) <- runFormPostNoNonce $ paramsFormlet Nothing catOpt
  case res of
    FormSuccess (Params title cat tag text) -> do
      runDB $ update id [ArticleTitle =. title, ArticleCat =. cat, ArticleContent =. text]
      runDB $ insert $ Tag tag id
      redirect RedirectTemporary $ ArticlesR $ category cat catOpt
    _ -> do
      redirect RedirectTemporary (EditArticleR id)

-- Helper functions
categories = do
  cas <- runDB $ selectList [] [Asc CategoryName]
  return $ map (\ c -> (categoryName $ snd c, fst c)) cas
  
category cat = fst . fromJust . find ((== cat) . snd)

tagsForArticle id = (intersperse ", ") . map (tagName . snd) . filter ((== id) . tagArticle . snd)
