{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Admin
       ( getAdminR
       , postAdminR
       ) where

import Foundation

import Control.Applicative
import Data.Text (Text, unpack, pack)
    
formletCat mparams html = (flip renderDivs) html $ Category
    <$> areq textField "Kategorie" (fmap categoryName mparams)
     
getAdminR :: Handler RepHtml
getAdminR = do
  requireAuth
  ((_, catForm), catEnctype) <- runFormGet $ formletCat Nothing
  defaultLayout $ do
   addWidget $(widgetFile "admin")

postAdminR :: Handler ()
postAdminR = do
  requireAuth
  ((catRes, _), _) <- runFormPostNoNonce $ formletCat Nothing
  case catRes of
    FormSuccess cat -> do
      runDB $ insert cat
      redirect RedirectTemporary AdminR
    _ -> do redirect RedirectTemporary AdminR
