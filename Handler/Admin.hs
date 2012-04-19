module Handler.Admin
       ( getAdminR
       , getDeleteCategoryR
       , postAdminR
       ) where

import Import
import Database.Persist.Store

formletCat :: Maybe Category -> Form Category
formletCat mparams = renderDivs $ Category
    <$> areq textField "Kategorie" (categoryName <$> mparams)

getAdminR :: Handler RepHtml
getAdminR = do
  requireAdmin
  cats <- runDB $ selectList [] [Asc CategoryName]
  ((res, catForm), catEnctype) <- runFormPost $ formletCat Nothing
  case res of
    FormSuccess cat -> do
      _ <- runDB $ insert cat
      redirect AdminR
    _ -> return ()
  defaultLayout $ do
    setTitle "Admin"
    $(widgetFile "admin")

postAdminR :: Handler RepHtml
postAdminR = getAdminR

getDeleteCategoryR :: CategoryId -> Handler ()
getDeleteCategoryR cid = do
  requireAdmin
  runDB $ deleteCascade cid
  redirect AdminR
