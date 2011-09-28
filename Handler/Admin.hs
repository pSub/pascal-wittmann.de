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
  ((res, catForm), catEnctype) <- runFormPost $ formletCat Nothing
  case res of
    FormSuccess cat -> do
      _ <- runDB $ insert cat
      redirect RedirectTemporary AdminR
    _ -> return ()
  defaultLayout $ do
    setTitle "Admin"
    addWidget $(widgetFile "admin")

postAdminR :: Handler RepHtml
postAdminR = getAdminR
   
getDeleteCategoryR :: CategoryId -> Handler ()
getDeleteCategoryR cid = do
  _ <- requireAuth
  runDB $ delete cid
  redirect RedirectTemporary AdminR
