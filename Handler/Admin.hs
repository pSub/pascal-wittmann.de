module Handler.Admin
       ( getAdminR
       , getDeleteCategoryR
       , postAdminR
       ) where

import Import

formletCat :: Maybe Category -> Html -> MForm Homepage Homepage (FormResult Category, Widget)
formletCat mparams html = (flip renderDivs) html $ Category
    <$> areq textField "Kategorie" (fmap categoryName mparams)

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
  eids <- runDB $ selectList [EntryCat ==. cid] []
  mapM_ (\ e -> runDB $ deleteWhere [CommentEntry ==. (entityKey e)]) eids
  mapM_ (\ e -> runDB $ deleteWhere [TaggedEntry ==. (entityKey e)]) eids
  mapM_ (\ e -> runDB $ deleteWhere [AttachmentEntry ==. (entityKey e)]) eids
  runDB $ deleteWhere [EntryCat ==. cid]
  runDB $ deleteWhere [TagCategory ==. cid]
  runDB $ delete cid
  redirect AdminR
