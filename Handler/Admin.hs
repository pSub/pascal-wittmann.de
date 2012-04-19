module Handler.Admin
       ( getAdminR
       , getDeleteCategoryR
       , postAdminR
       ) where

import Import
import Database.Persist.Store
import Control.Arrow

formletCat :: Maybe Category -> Form Category
formletCat mparams = renderDivs $ Category
    <$> areq textField "Kategorie" (categoryName <$> mparams)

deleteCatForm :: [(Text, CategoryId)] -> Form [CategoryId]
deleteCatForm cats = renderDivs $ areq (multiSelectFieldList cats) "" Nothing

getAdminR :: Handler RepHtml
getAdminR = do
  requireAdmin
  ((res, catForm), catEnctype) <- runFormPost $ formletCat Nothing
  case res of
    FormSuccess cat -> do
      _ <- runDB $ insert cat
      redirect AdminR
    _ -> return ()

  cats <- runDB $ selectList [] [Asc CategoryName]
  let opts = map ((categoryName . entityVal) &&& entityKey) cats
  -- FIXME: used formGet because the identifiers of both forms clash
  ((deleteRes, deleteCatForm), deleteEnctype) <- runFormGet $ deleteCatForm opts
  case deleteRes of
    FormSuccess ckeys -> do
      runDB $ deleteCascadeWhere [FilterOr $ map (CategoryId ==.) ckeys]
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
