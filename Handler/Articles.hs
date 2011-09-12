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
getArticlesR catName = do
  mu <- maybeAuth
  mcat <- runDB $ getBy $ CategoryUniq catName
  tags <- runDB $ selectList [TagCategory ==. (fst $ fromJust mcat)] [Asc TagName]
  articles <- runDB $ selectList [ArticleCat ==. (fst $ fromJust mcat)] [Desc ArticleDate]
  defaultLayout $ do
  setTitle "Log"
  addCassius $(cassiusFile "articles")
  addWidget $(widgetFile "articles")

getNewArticleR :: Handler RepHtml
getNewArticleR = do
  _ <- requireAuth
  catOpt <- categories
  ((_, form), enctype) <- runFormGet $ paramsFormlet Nothing catOpt
  defaultLayout $ do
    addWidget $(widgetFile "new-article")


postNewArticleR :: Handler ()
postNewArticleR = do
  _ <- requireAuth
  catOpt <- categories
  ((res, _), _) <- runFormPostNoNonce $ paramsFormlet Nothing catOpt
  case res of
    FormSuccess p -> do
      now <- liftIO getCurrentTime
      aid <- runDB $ insert $ Article (title p) (text p) (cat p) "" now
      _ <- runDB $ insert $ Tag (tag p) aid (cat p)
      redirect RedirectTemporary $ ArticlesR $ category (cat p) catOpt
    _ -> do
      redirect RedirectTemporary NewArticleR

getEditArticleR :: ArticleId -> Handler RepHtml
getEditArticleR aid = do
  _ <- requireAuth
  ma <- runDB $ get aid
  catOpt <- categories
  case ma of
    Just a ->
      do ((_, form), enctype) <- runFormGet $ paramsFormlet (Just $ Params (articleTitle a) (articleCat a) "" (articleContent a)) catOpt
         defaultLayout $ do
         addWidget $(widgetFile "new-article")
    Nothing -> do
      redirect RedirectTemporary NewArticleR
    
postEditArticleR :: ArticleId -> Handler ()
postEditArticleR aid = do
  _ <- requireAuth
  catOpt <- categories
  ((res, _), _) <- runFormPostNoNonce $ paramsFormlet Nothing catOpt
  case res of
    FormSuccess p -> do
      runDB $ update aid [ArticleTitle =. (title p), ArticleCat =. (cat p), ArticleContent =. (text p)]
      _ <- runDB $ insert $ Tag (tag p) aid (cat p)
      redirect RedirectTemporary $ ArticlesR $ category (cat p) catOpt
    _ -> do
      redirect RedirectTemporary (EditArticleR aid)

-- Helper functions
categories = do
  cas <- runDB $ selectList [] [Asc CategoryName]
  return $ map (\ c -> (categoryName $ snd c, fst c)) cas

category c = fst . fromJust . find ((== c) . snd)

tagsForArticle aid = (intersperse ", ") . map (tagName . snd) . filter ((== aid) . tagArticle . snd)
