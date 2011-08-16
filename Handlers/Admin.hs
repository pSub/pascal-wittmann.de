{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Admin
       ( getAdminR
       , postAdminR
       ) where

import Homepage
import qualified Settings
import Model
import Yesod
import Yesod.Auth

import Control.Applicative
import Data.Text (Text, unpack, pack)
     
--formletTag mparams = fieldsToTable $ Tag
--    <$> stringField "Tag" (fmap tagName mparams)
    
formletCat mparams = fieldsToTable $ Category
    <$> stringField "Kategorie" (fmap categoryName mparams)
     
getAdminR :: Handler RepHtml
getAdminR = do
  requireAuth
  --(_, tagForm, tagEnctype) <- runFormGet $ formletTag Nothing
  (_, catForm, catEnctype) <- runFormGet $ formletCat Nothing
  defaultLayout $ do
    $(Settings.hamletFile "admin")

postAdminR :: Handler ()
postAdminR = do
  requireAuth
--  (tagRes, _, _) <- runFormPostNoNonce $ formletTag Nothing
--  case tagRes of
--    FormSuccess tag -> do
--      runDB $ insert tag
--      redirect RedirectTemporary AdminR
--    _ -> do redirect RedirectTemporary AdminR
            
  (catRes, _, _) <- runFormPostNoNonce $ formletCat Nothing
  case catRes of
    FormSuccess cat -> do
      runDB $ insert cat
      redirect RedirectTemporary AdminR
    _ -> do redirect RedirectTemporary AdminR
