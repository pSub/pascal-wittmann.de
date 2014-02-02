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

import           Control.Arrow         ((&&&))
import           Data.List             (intersperse, sort)
import qualified Data.List             as L (delete)
import           Data.Maybe            hiding (isNothing)
import qualified Data.Maybe            as M
import           Data.Text             (pack, splitOn, strip, unpack)
import qualified Data.Text             as T
import           Data.Time
import           Data.Tree
import           Import
import           Prelude               hiding (unwords)
import qualified Settings
import           System.Directory
import           System.FilePath.Posix
import           Text.Regex
import           Yesod.Markdown

data PEntry = PEntry
     { title :: Text
     , ident :: Text
     , cat   :: CategoryId
     , tag   :: Text
     , recap :: Text
     , text  :: Markdown
     }

-- Simple regex to match urls
urlRegex = mkRegex "\\b(https?|ftp|file)://[-A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]"

-- | Form for entering a new entry.
entryForm ::  Maybe PEntry -> CategoryId -> Form PEntry
entryForm mparams category = renderDivs $ PEntry
    <$> areq textField (fieldSettingsLabel MsgTitle) (title <$> mparams)
    <*> areq textField (fieldSettingsLabel MsgIdent) (ident <$> mparams)
    <*> pure category
    <*> areq textField (fieldSettingsLabel MsgTags) (tag <$> mparams)
    <*> areq textField (fieldSettingsLabel MsgSummary) (recap <$> mparams)
    <*> areq markdownField  (fieldSettingsLabel MsgText) (text <$> mparams)

-- | Form to add comments to an entry
commentForm :: Bool -> Maybe Text -> Markdown -> UTCTime -> Maybe CommentId -> EntryId -> Form Comment
commentForm loggedin author comment now parentKey entryKey = renderDivs $ Comment
    <$> aopt textField (fieldSettingsLabel MsgName) (Just author)
    <*> areq noURLsMarkdownField (fieldSettingsLabel MsgComment) (Just comment)
    <*> aopt doNotFillHiddenField "" Nothing
    <*> pure now
    <*> pure parentKey
    <*> pure entryKey
    <*> pure False -- If an entry is edited it becomes visible again.
  where doNotFillHiddenField = checkBool T.null ("You Shall Not Pass!!!" :: Text) hiddenField
        noURLsMarkdownField = checkBool (\t -> loggedin || matchRegex urlRegex (unpack $ unMarkdown t) == Nothing)
                                    ("URLs are not allowed." :: Text)
                                    markdownField

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
getEntriesR :: Text -> Handler Html
getEntriesR catName = getEntriesByTagR catName []

-- | Central handler for showing a list of entries of a category.
-- Optionally filtered by tags.
-- The first argument is the category name, the second is a
-- list of tags. If this list is not empty, every entry which is assigned
-- to at least one tag from the list (given it is in the right category)
-- is shown
getEntriesByTagR :: Text -> [Text] -> Handler Html
getEntriesByTagR catName tagNames = do
  mu <- maybeAdmin
  category <- runDB $ getBy404 $ UniqueCategory catName
  currentTags <- mapMaybe (entityKey <$>) <$> mapM (\ n -> runDB $ getBy $ UniqueTag n (entityKey category)) tagNames
  tagging <- runDB $ select $ from $ \(t `InnerJoin` s) -> do
                     on $ t ^. TagId ==. s ^. TaggedTag
                     orderBy [asc (t ^. TagName)]
                     return (t, s)

  tags <- runDB $ select $ from $ \t -> where_ (t ^. TagCategory ==. val (entityKey category)) >> return t
  comments <- map (\(Value e, Value c) -> (e, c)) <$> (runDB $ select $ from $ \(e `InnerJoin` c) -> do
                      on $ e ^. EntryId ==. c ^. CommentEntry
                      where_ (c ^. CommentDeleted ==. val False)
                      groupBy $ e ^. EntryId
                      return (e ^. EntryId, countRows))

  entries <- if null tagNames
                then runDB $ select $ from $ \e -> do
                             where_ (e ^. EntryCat ==. val (entityKey category))
                             orderBy [desc (e ^. EntryDate)]
                             return e
                else runDB $ select $ from $ \(e `InnerJoin` t) -> do
                             on $ e ^. EntryId ==. t ^. TaggedEntry
                             mapM_ (where_ . (t ^. TaggedTag ==.) . val) currentTags
                             return e
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
entryHandler :: Text -> Text -> Maybe CommentId -> Handler Html
entryHandler catName curIdent mparent = do
  mu <- maybeAdmin
  mua <- maybeAuth
  entry <- runDB $ getBy404 $ UniqueEntry curIdent
  atts <- runDB $ select $ from $ \a -> do
                  where_ (a ^. AttachmentEntry ==. val (entityKey entry))
                  orderBy [asc (a ^. AttachmentDescr)]
                  return a
  comments <- buildComments <$> (runDB $ select $ from $ \c -> do
                                         where_ (c ^. CommentEntry ==. val (entityKey entry))
                                         where_ (c ^. CommentDeleted ==. val False)
                                         orderBy [asc (c ^. CommentDate)]
                                         return c)
  now <- liftIO getCurrentTime
  ((_, formNew), _) <- runFormPost $ commentForm (isJust mua) Nothing "" now Nothing (entityKey entry)
  ((res, formEdit), enctype) <- runFormPost $ commentForm (isJust mua) (maybe Nothing (userName . entityVal) mua) "" now mparent (entityKey entry)
  case res of
    FormSuccess comment -> do
      _ <- runDB $ insert comment
      redirect $ EntryR catName curIdent
    _ -> return ()
  defaultLayout $ do
    setTitle $ toHtml $ entryTitle $ entityVal entry
    $(widgetFile "entry")

getEntryCommentR :: Text -> Text -> CommentId -> Handler Html
getEntryCommentR catName curIdent curParent = entryHandler catName curIdent (Just curParent)

postEntryCommentR :: Text -> Text -> CommentId -> Handler Html
postEntryCommentR = getEntryCommentR

getEntryR :: Text -> Text ->  Handler Html
getEntryR catName curIdent = entryHandler catName curIdent Nothing

postEntryR :: Text -> Text -> Handler Html
postEntryR = getEntryR

-- | Handler that deletes a Tag from an entry. The entry
-- is identified by its Ident.
getDeleteTagR :: Text -> TagId -> Handler ()
getDeleteTagR category tid = do
  requireAdmin
  runDB $ deleteKey tid
  redirect $  EntriesR category

-- | This handler builds a page with a form to
-- create a new article. It needs the identifier
-- of the Category, the entry should be placed in.
getNewEntryR :: Text -> Handler Html
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

postNewEntryR :: Text -> Handler Html
postNewEntryR = getNewEntryR

-- | Handler that fills all fields of an entry
-- that is specified by the second parameter.
-- The first parameter is the Category.
getEditEntryR :: Text -> Text -> Handler Html
getEditEntryR catName eid = do
  requireAdmin
  Entity eKey eVal <- runDB $ getBy404 $ UniqueEntry eid
  tags <- showTags <$> (runDB $ select $ from $ \(t `InnerJoin` s) -> do
                                on $ t ^. TaggedTag ==. s ^. TagId
                                where_ $ t ^. TaggedEntry ==. val eKey
                                orderBy [asc (s ^. TagName)]
                                return s)
  ((res, form), enctype) <- runFormPost $ entryForm (Just $ PEntry (entryTitle eVal)
                                                                   (entryIdent eVal)
                                                                   (entryCat eVal)
                                                                    tags
                                                                   (entryRecap eVal)
                                                                   (entryContent eVal))
                                                    (entryCat eVal)
  case res of
      FormSuccess p -> do
        category <- runDB $ get404 $ cat p
        now <- liftIO $ getCurrentTime
        runDB $ update $ \e -> do
                set e [ EntryTitle =. val (title p)
                      , EntryIdent =. val (ident p)
                      , EntryContent =. val (text p)
                      , EntryRecap =. val (recap p)
                      , EntryLastMod =. val now]
                where_ (e ^. EntryId ==. val eKey)
        runDB $ delete $ from $ \t -> where_ (t ^. TaggedEntry ==.  val eKey)
        insertTags (cat p) (eKey) (buildTagList p)
        redirect $  EntryR (categoryName category) (ident p)
      _ -> return ()
  defaultLayout $ do
       setTitle "Edit Entry"
       $(widgetFile "new-entry")
  where showTags = pack . concat . intersperse ", " . map (unpack . tagName . entityVal)

buildTagList :: PEntry -> [Text]
buildTagList = map strip . splitOn "," . tag

postEditEntryR :: Text -> Text -> Handler Html
postEditEntryR = getEditEntryR

getDeleteEntryR :: Text -> EntryId -> Handler ()
getDeleteEntryR category eid = do
  requireAdmin
  runDB $ deleteCascade eid
  redirect $ EntriesR category

getDeleteCommentR :: Text -> Text -> CommentId -> Handler ()
getDeleteCommentR catName curIdent cid = do
  requireAdmin
  runDB $ update $ \c -> do
        set c [CommentDeleted =. val True]
        where_ (c ^. CommentId ==. val cid)
  redirect $ EntryR catName curIdent

getUploadFileR :: Text -> Text -> Handler Html
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
  atts <- runDB $ select $ from $ \a -> do
                  where_ (a ^. AttachmentEntry ==. val (entityKey e))
                  orderBy [asc (a ^. AttachmentDescr)]
                  return a
  ((resDelete, deleteFileFormView), _) <- runFormPost $ deleteFileForm $ map ((attachmentDescr . entityVal) &&& entityKey) atts
  case resDelete of
       FormSuccess aids -> do
         as <- runDB $ select $ from $ \a -> do
                       mapM_ (where_ . (a ^. AttachmentId ==.) . val ) aids
                       return a
         runDB $ delete $ from $ \a -> mapM_ (where_ . (a ^. AttachmentId ==.) . val) aids
         liftIO $ mapM_ (removeFile . buildFileName . attachmentFile . entityVal) as
         redirect $ EntryR catName curIdent
       _ -> return ()
  defaultLayout $(widgetFile "upload-file")

postUploadFileR :: Text -> Text -> Handler Html
postUploadFileR = getUploadFileR

getMoveFileR :: Text -> Text -> AttachmentId -> Handler Html
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

postMoveFileR :: Text -> Text -> AttachmentId -> Handler Html
postMoveFileR = getMoveFileR

-- Helper functions
buildFileName :: Text -> FilePath
buildFileName name = Settings.staticDir ++ [pathSeparator] ++ unpack name

tagsForEntry :: Key Entry -> [(b, Entity Tagged)] -> [b]
tagsForEntry eid = map fst . filter (((== eid) . taggedEntry . entityVal) . snd)

insertTags :: CategoryId -> EntryId -> [Text] -> Handler ()
insertTags category eid = mapM_ insertTag . filter (not . T.null)
           where insertTag t = do
                  mtag <- runDB $ getBy $ UniqueTag t category
                  tid <- (runDB $ insert $ Tag t category) -|- (entityKey <$> mtag)
                  runDB $ insert $ Tagged tid eid

buildComments :: [Entity Comment] -> [(Integer, Entity Comment)]
buildComments cs = concat $ map flatten $ unfoldForest (id &&& getChilds) roots
      where
        roots = zip [0,0..] (filter (M.isNothing . commentParent . entityVal) cs)
        getChilds (depth, c) = zip (repeat $ succ depth) (filter (isChild c) cs)
        isChild (Entity key _) (Entity _ value) = maybe False (key ==) (commentParent value)

createStaticRoute :: Text -> StaticRoute
createStaticRoute name = StaticRoute [name] []

(-|-) :: Monad m => m a -> Maybe a -> m a
_ -|- Just a = return a
action -|- Nothing = action
