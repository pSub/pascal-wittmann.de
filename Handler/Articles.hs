{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Articles
       ( getArticlesR
       , getArticleR
       , getNewArticleR
       , getDeleteTagR
       , getDeleteArticleR
       , postNewArticleR
       , getEditArticleR
       , postEditArticleR
       ) where

import Foundation
import Yesod.Goodies.Markdown

import Control.Applicative
import Data.List (find, intersperse)
import Data.Time
import Data.Text (Text, unpack, pack)
import Data.Maybe
import Data.List.Split (splitOn)

data Params = Params
     { title :: Text
     , ident :: Text
     , cat :: CategoryId
     , tag :: Text
     , text :: Markdown
     } deriving Show
     
paramsFormlet ::  Maybe Params -> [(Text, CategoryId)] -> Html -> Form Homepage Homepage (FormResult Params, Widget)
paramsFormlet mparams cats html = (flip renderDivs) html $ Params
    <$> areq textField "Title" (title <$> mparams)
    <*> areq textField "Ident" (ident <$> mparams)
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

getArticleR :: Text -> Text -> Handler RepHtml
getArticleR catName ident = do
  mu <- maybeAuth
  marticle <- runDB $ getBy $ ArticleUniq ident
  tags <- runDB $ selectList [TagArticle ==. (fst $ fromJust marticle)] [Asc TagName]
  defaultLayout $ do
    addCassius $(cassiusFile "article")
    addWidget $(widgetFile "article")
    
getDeleteTagR :: Text -> TagId -> Handler ()
getDeleteTagR category tid = do
  _ <- requireAuth
  runDB $ delete tid
  redirect RedirectTemporary $ ArticlesR category

getNewArticleR :: Handler RepHtml
getNewArticleR = do
  _ <- requireAuth
  catOpt <- categories
  tags <- return []
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
      aid <- runDB $ insert $ Article (title p) (ident p) (text p) (cat p) "" now
      insertTags (cat p) aid $ splitOn "," $ filter (/= ' ') (unpack $ tag p)
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
      do ((_, form), enctype) <- runFormGet $ paramsFormlet (Just $ Params (articleTitle a) (articleIdent a) (articleCat a) "" (articleContent a)) catOpt
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
      runDB $ update aid [ArticleTitle =. (title p), ArticleIdent =. (ident p), ArticleCat =. (cat p), ArticleContent =. (text p)]
      insertTags (cat p) aid $ splitOn "," $ filter (/= ' ') (unpack $ tag p)
      redirect RedirectTemporary $ ArticlesR $ category (cat p) catOpt
    _ -> do
      redirect RedirectTemporary (EditArticleR aid)
      
getDeleteArticleR :: Text -> ArticleId -> Handler ()
getDeleteArticleR category aid = do
  _ <- requireAuth
  runDB $ deleteWhere [TagArticle ==. aid]
  runDB $ delete aid
  redirect RedirectTemporary $ ArticlesR category

-- Helper functions
categories = do
  cas <- runDB $ selectList [] [Asc CategoryName]
  return $ map (\ c -> (categoryName $ snd c, fst c)) cas

category c = fst . fromJust . find ((== c) . snd)

tagsForArticle aid = filter ((== aid) . tagArticle . snd)

insertTags category aid (t:tags) = do
  _ <- runDB $ insert $ Tag (pack t) aid category
  insertTags category aid tags
insertTags _ _ [] = return ()
