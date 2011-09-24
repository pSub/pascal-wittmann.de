{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Admin
       ( getAdminR
       , getDeleteCategoryR
       , postAdminR
       ) where

import Foundation
import Control.Applicative

formletCat :: Maybe Category -> Html -> Form Homepage Homepage (FormResult Category, Widget)
formletCat mparams html = (flip renderDivs) html $ Category
    <$> areq textField "Kategorie" (fmap categoryName mparams)
     
getAdminR :: Handler RepHtml
getAdminR = do
  _ <- requireAuth
  cats <- runDB $ selectList [] [Asc CategoryName]
  ((_, catForm), catEnctype) <- runFormGet $ formletCat Nothing
  defaultLayout $ do
    setTitle "Admin"
    addWidget $(widgetFile "admin")
   
getDeleteCategoryR :: CategoryId -> Handler ()
getDeleteCategoryR cid = do
  _ <- requireAuth
  runDB $ delete cid
  redirect RedirectTemporary AdminR

postAdminR :: Handler ()
postAdminR = do
  _ <- requireAuth
  ((catRes, _), _) <- runFormPostNoNonce $ formletCat Nothing
  case catRes of
    FormSuccess cat -> do
      _ <- runDB $ insert cat
      redirect RedirectTemporary AdminR
    _ -> do redirect RedirectTemporary AdminR

