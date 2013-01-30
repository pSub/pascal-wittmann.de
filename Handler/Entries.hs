module Handler.Entries
       ( getEntriesR
       , getEntriesByTagR
       , getEntryR
       , postEntryR
       , getEntryCommentR
       , postEntryCommentR
       , getNewEntryR
       , getDeleteTagR
       , getDeleteEntryR
       , postNewEntryR
       , getEditEntryR
       , postEditEntryR
       , getDeleteCommentR
       , postUploadFileR
       , getUploadFileR
       , getMoveFileR
       , postMoveFileR
       ) where

import           Import
import qualified Settings
import           Yesod.RST
import           Yesod.Markdown
import           Database.Persist.Store (deleteCascade)
import           Database.Persist.Query.Join     (selectOneMany, SelectOneMany(..))
import           Database.Persist.Query.Join.Sql (runJoin)

import           Control.Arrow ((&&&))
import           Data.List                       (intersperse, sort)
import qualified Data.List                       as L (delete)
import           Data.Maybe
import           Data.Text                       (unpack, pack, strip, splitOn)
import qualified Data.Text                       as T
import           Data.Time
import           Data.Tree
import           Prelude                         hiding (unwords)
import           System.Directory
import           System.FilePath.Posix

data PEntry = PEntry
     { title :: Text
     , ident :: Text
     , cat :: CategoryId
     , tag :: Text
     , recap :: Text
     , text :: RST
     }

-- | Form for entering a new entry.
entryForm ::  Maybe PEntry -> CategoryId -> Form PEntry
entryForm mparams category = renderDivs $ PEntry
    <$> areq textField (fieldSettingsLabel MsgTitle) (title <$> mparams)
    <*> areq textField (fieldSettingsLabel MsgIdent) (ident <$> mparams)
    <*> pure category
    <*> areq textField (fieldSettingsLabel MsgTags) (tag <$> mparams)
    <*> areq textField (fieldSettingsLabel MsgSummary) (recap <$> mparams)
    <*> areq rstField  (fieldSettingsLabel MsgText) (text <$> mparams)

-- | Form to add comments to an entry
commentForm :: Maybe Text -> Markdown -> UTCTime -> Maybe CommentId -> EntryId -> Form Comment
commentForm author comment now parentKey entryKey = renderDivs $ Comment
    <$> aopt textField (fieldSettingsLabel MsgName) (Just author)
    <*> areq markdownField (fieldSettingsLabel MsgComment) (Just comment)
    <*> pure now
    <*> pure parentKey
    <*> pure entryKey
    <*> pure False -- If an entry is edited it becomes visible again.

-- | Form to upload attachments to an entry
fileForm :: Text -> Form (FileInfo, Text)
fileForm name = renderDivs $ (,)
         <$> fileAFormReq (fieldSettingsLabel MsgAttachment)
         <*> areq textField (fieldSettingsLabel MsgName) (Just name)

-- | Form to delete (multiple) attachments
deleteFileForm :: [(Text, AttachmentId)] -> Form ([AttachmentId])
deleteFileForm atts = renderDivs $ areq (multiSelectFieldList atts) "" Nothing

-- | Calls the central entries handler ('getEntriesByTag') without a restriction
-- on tags.
getEntriesR :: Text -> Handler RepHtml
getEntriesR catName = getEntriesByTagR catName []

-- | Central handler for showing a list of entries of a category.
-- Optionally filtered by tags.
-- The first argument is the category name, the second is a
-- list of tags. If this list is not empty, every entry which is assigned
-- to at least one tag from the list (given it is in the right category)
-- is shown
getEntriesByTagR :: Text -> [Text] -> Handler RepHtml
getEntriesByTagR catName tagNames = do
  mu <- maybeAdmin
  category <- runDB $ getBy404 $ UniqueCategory catName
  currentTags <- mapMaybe (entityKey <$>) <$> mapM (\ n -> runDB $ getBy $ UniqueTag n (entityKey category)) tagNames
  tagsEntries <- runDB $ runJoin (selectOneMany (TaggedTag <-.) taggedTag)
          { somFilterOne = [TagCategory ==. (entityKey category)]
          , somOrderOne = [Asc TagName]
          }
  tags <- return $ map fst tagsEntries
  comments <- map (\ (e,c) -> (entityKey e, length c)) <$> (runDB $ runJoin (selectOneMany (CommentEntry <-.) commentEntry)
                                                                  { somFilterMany = [CommentDeleted ==. False]})
  entries <- if null tagNames
                then runDB $ selectList [EntryCat ==. (entityKey category)] [Desc EntryDate]
                else map fst <$> (runDB $ runJoin (selectOneMany (TaggedEntry <-.) taggedEntry)
                     { somFilterMany = [FilterOr . map (TaggedTag ==.) $ currentTags]
                     , somOrderOne = [Desc EntryDate]
                     })
  defaultLayout $ do
    if null tagNames
       then setTitle $ toHtml catName
       else setTitle $ toHtml $ catName <> " :: " <> (T.concat $ intersperse ", " tagNames)
    $(widgetFile "entries")

-- | If the element is not contained in the list it is added,
-- otherwise deleted from the list.
toggleTag :: Eq a => a -> [a] -> [a]
toggleTag t ts
  | t `elem` ts = L.delete t ts
  | otherwise = t:ts

-- | This handler is responsible for building pages showing an
-- entry in full length including attachments and comments.
entryHandler :: Text -> Text -> Maybe CommentId -> Handler RepHtml
entryHandler catName curIdent mparent = do
  mu <- maybeAdmin
  mua <- maybeAuth
  entry <- runDB $ getBy404 $ UniqueEntry curIdent
  tags <- runDB $ runJoin $ (selectOneMany (TaggedTag <-.) taggedTag)
          { somFilterMany = [TaggedEntry ==. (entityKey entry)]
          , somOrderOne = [Asc TagName]
          }
  atts <- runDB $ selectList [AttachmentEntry ==. (entityKey entry)] [Asc AttachmentDescr]
  comments <- buildComments <$> (runDB $ selectList [CommentEntry ==. (entityKey entry), CommentDeleted ==. False] [Asc CommentDate])

  now <- liftIO getCurrentTime
  ((_, formNew), _) <- runFormPost $ commentForm Nothing "" now Nothing (entityKey entry)
  ((res, formEdit), enctype) <- runFormPost $ commentForm (maybe Nothing (userName . entityVal) mua) "" now mparent (entityKey entry)
  case res of
    FormSuccess comment -> do
      _ <- runDB $ insert comment
      redirect $ EntryR catName curIdent
    _ -> return ()
  defaultLayout $ do
    setTitle $ toHtml $ entryTitle $ entityVal entry
    $(widgetFile "entry")

getEntryCommentR :: Text -> Text -> CommentId -> Handler RepHtml
getEntryCommentR catName curIdent curParent = entryHandler catName curIdent (Just curParent)

postEntryCommentR :: Text -> Text -> CommentId -> Handler RepHtml
postEntryCommentR = getEntryCommentR

getEntryR :: Text -> Text ->  Handler RepHtml
getEntryR catName curIdent = entryHandler catName curIdent Nothing

postEntryR :: Text -> Text -> Handler RepHtml
postEntryR = getEntryR


-- | Handler that deletes a Tag from an entry. The entry
-- is identified by its Ident.
getDeleteTagR :: Text -> TagId -> Handler ()
getDeleteTagR category tid = do
  requireAdmin
  runDB $ delete tid
  redirect $  EntriesR category

-- | This handler builds a page with a form to
-- create a new article. It needs the identifier
-- of the Category, the entry should be placed in.
getNewEntryR :: Text -> Handler RepHtml
getNewEntryR catName = do
  requireAdmin
  category <- runDB $ getBy404 $ UniqueCategory catName
  ((res, form), enctype) <- runFormPost $ entryForm Nothing (entityKey category)
  case res of
    FormSuccess p -> do
      now <- liftIO getCurrentTime
      aid <- runDB $ insert $ Entry (title p) (ident p) (text p) (cat p) (recap p) "" now now
      insertTags (cat p) aid (buildTagList p)
      redirect $  EntriesR $ categoryName $ entityVal category
    _ -> return ()
  defaultLayout $ do
    setTitle "New Entry"
    $(widgetFile "new-entry")

postNewEntryR :: Text -> Handler RepHtml
postNewEntryR = getNewEntryR


-- | Handler that fills all fields of an entry
-- that is specified by the second parameter.
-- The first parameter is the Category.
getEditEntryR :: Text -> Text -> Handler RepHtml
getEditEntryR catName eid = do
  requireAdmin
  Entity eKey eVal <- runDB $ getBy404 $ UniqueEntry eid
  tags <- showTags <$> (runDB $ runJoin $ (selectOneMany (TaggedTag <-.) taggedTag)
          { somFilterMany = [TaggedEntry ==. eKey]
          , somOrderOne = [Asc TagName]
          })
  ((res, form), enctype) <- runFormPost $ entryForm (Just $ PEntry (entryTitle eVal) (entryIdent eVal) (entryCat eVal) tags (entryRecap eVal) (entryContent eVal)) (entryCat eVal)
  case res of
      FormSuccess p -> do
        category <- runDB $ get404 $ cat p
        now <- liftIO $ getCurrentTime
        runDB $ update eKey [EntryTitle =. (title p), EntryIdent =. (ident p), EntryContent =. (text p), EntryRecap =. (recap p), EntryLastMod =. now]
        runDB $ deleteWhere [TaggedEntry ==. eKey]
        insertTags (cat p) (eKey) (buildTagList p)
        redirect $  EntryR (categoryName category) (ident p)
      _ -> return ()
  defaultLayout $ do
       setTitle "Edit Entry"
       $(widgetFile "new-entry")
  where showTags = pack . concat . intersperse ", " . map (unpack . tagName . entityVal . fst)

buildTagList :: PEntry -> [Text]
buildTagList = map strip . splitOn "," . tag

postEditEntryR :: Text -> Text -> Handler RepHtml
postEditEntryR = getEditEntryR

getDeleteEntryR :: Text -> EntryId -> Handler ()
getDeleteEntryR category eid = do
  requireAdmin
  runDB $ deleteCascade eid
  redirect $  EntriesR category

getDeleteCommentR :: Text -> Text -> CommentId -> Handler ()
getDeleteCommentR catName curIdent cid = do
  requireAdmin
  runDB $ update cid [CommentDeleted =. True]
  redirect $  EntryR catName curIdent

getUploadFileR :: Text -> Text -> Handler RepHtml
getUploadFileR catName curIdent = do
  requireAdmin
  e <- runDB $ getBy404 $ UniqueEntry curIdent
  ((resCreate, newFileFormView), enctype) <- runFormPost $ fileForm ""
  case resCreate of
       FormSuccess (file, descr) -> do
          now <- liftIO $ getCurrentTime
          _ <- runDB $ insert $ Attachment (fileName file) (entityKey e) descr now
          liftIO $ fileMove file (buildFileName $ fileName file)
          redirect $  EntryR catName curIdent
       _ -> return ()
  atts <- runDB $ selectList [AttachmentEntry ==. (entityKey e)] [Asc AttachmentDescr]
  ((resDelete, deleteFileFormView), _) <- runFormPost $ deleteFileForm $ map ((attachmentDescr . entityVal) &&& entityKey) atts
  case resDelete of
       FormSuccess aids -> do
         as <- runDB $ selectList [FilterOr $ map (AttachmentId ==.) aids] []
         runDB $ deleteWhere [FilterOr $ map (AttachmentId ==.) aids]
         liftIO $ mapM_ (removeFile . buildFileName . attachmentFile . entityVal) as
         redirect $ EntryR catName curIdent
       _ -> return ()
  defaultLayout $
      $(widgetFile "upload-file")

postUploadFileR :: Text -> Text -> Handler RepHtml
postUploadFileR = getUploadFileR

getMoveFileR :: Text -> Text -> AttachmentId -> Handler RepHtml
getMoveFileR catName curIdent aid = do
  requireAdmin
  a <- runDB $ get404 aid
  ((res, form), enctype) <- runFormPost $ fileForm $ attachmentDescr a
  case res of
       FormSuccess (file, descr) -> do
          now <- liftIO $ getCurrentTime
          _ <- runDB $ insert $ Attachment (fileName file) (attachmentEntry a) descr now
          liftIO $ fileMove file (buildFileName $ fileName file)
          redirect $  EntryR catName curIdent
       _ -> return ()
  defaultLayout $
      $(widgetFile "move-file")

postMoveFileR :: Text -> Text -> AttachmentId -> Handler RepHtml
postMoveFileR = getMoveFileR

-- Helper functions
buildFileName :: Text -> FilePath
buildFileName name = Settings.staticDir ++ [pathSeparator] ++ unpack name

tagsForEntry :: Key Entry -> [(b, [Entity Tagged])] -> [b]
tagsForEntry eid = map fst . filter (any ((== eid) . taggedEntry . entityVal) . snd)

insertTags :: CategoryId -> EntryId -> [Text] -> Handler ()
insertTags category eid = mapM_ insertTag . filter (not . T.null)
           where insertTag t = do
                 mtag <- runDB $ getBy $ UniqueTag t category
                 tid <- (runDB $ insert $ Tag t category) -|- (entityKey <$> mtag)
                 runDB $ insert $ Tagged tid eid

buildComments :: [Entity Comment] -> [(Integer, Entity Comment)]
buildComments cs = concat $ map flatten $ unfoldForest (id &&& getChilds) roots
      where
        roots = zip [0,0..] (filter (isNothing . commentParent . entityVal) cs)
        getChilds (depth, c) = zip (repeat $ succ depth) (filter (isChild c) cs)
        isChild (Entity key _) (Entity _ val) = maybe False (key ==) (commentParent val)

createStaticRoute :: Text -> StaticRoute
createStaticRoute name = StaticRoute [name] []

(-|-) :: Monad m => m a -> Maybe a -> m a
_ -|- Just a = return a
action -|- Nothing = action
