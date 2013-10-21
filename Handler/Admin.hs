module Handler.Admin
       ( getAdminR
       , getDeleteCategoryR
       , postAdminR
       ) where

import           Control.Arrow ((&&&))
import           Import

formletCat :: Maybe Category -> Form Category
formletCat mparams = renderDivs $ Category
    <$> areq textField (fieldSettingsLabel MsgCategory) (categoryName <$> mparams)

deleteCatFormlet :: [(Text, CategoryId)] -> Form [CategoryId]
deleteCatFormlet cats = renderDivs $ areq (multiSelectFieldList cats) "" Nothing

getAdminR :: Handler Html
getAdminR = do
  requireAdmin
  ((res, catForm), catEnctype) <- runFormPost $ formletCat Nothing
  case res of
    FormSuccess cat -> do
      _ <- runDB $ insert cat
      redirect AdminR
    _ -> return ()

  cats <- runDB $ select $ from $ \c -> orderBy [asc (c ^. CategoryName)] >> return c
  let opts = map ((categoryName . entityVal) &&& entityKey) cats
  -- FIXME: used formGet because the identifiers of both forms clash
  ((deleteRes, deleteCatForm), deleteEnctype) <- runFormGet $ deleteCatFormlet opts
  case deleteRes of
    FormSuccess ckeys -> do
      -- FIXME: Can this be unified into one query, as it was with `deleteCascadeWhere`?
      runDB $ mapM_ deleteCascade ckeys
      redirect AdminR
    _ -> return ()

  defaultLayout $ do
    setTitle "Admin"
    $(widgetFile "admin")

postAdminR :: Handler Html
postAdminR = getAdminR

getDeleteCategoryR :: CategoryId -> Handler ()
getDeleteCategoryR cid = do
  requireAdmin
  runDB $ deleteCascade cid
  redirect AdminR
