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
       , getDeleteFileR
       ) where

import           Import
import qualified Settings
import           Yesod.RST
import           Yesod.Markdown
import           Yesod.Static
import           Database.Persist.Store (deleteCascade)
import           Database.Persist.Query.Join     (selectOneMany, SelectOneMany(..))
import           Database.Persist.Query.Join.Sql (runJoin)

import qualified Data.ByteString.Lazy            as BS (writeFile)
import           Data.List                       (intersperse, sort)
import qualified Data.List                       as L (delete)
import           Data.Maybe
import           Data.Text                       (unpack, pack, append, strip, splitOn)
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

data PComment = PComment
     { parent :: Maybe CommentId
     , author :: Maybe Text
     , content :: Markdown
     }

entryForm ::  Maybe PEntry -> CategoryId -> Form PEntry
entryForm mparams category = renderDivs $ PEntry
    <$> areq textField "Title" (title <$> mparams)
    <*> areq textField "Ident" (ident <$> mparams)
    <*> pure category
    <*> areq textField "Tags" (tag <$> mparams)
    <*> areq textField "Summary" (recap <$> mparams)
    <*> areq rstField "Text" (text <$> mparams)

commentForm :: Maybe PComment -> Maybe CommentId -> Form PComment
commentForm mcomment mparent = renderDivs $ PComment
    <$> pure mparent
    <*> aopt textField "Name" (author <$> mcomment)
    <*> areq markdownField "Kommentar" (content <$> mcomment)

fileForm :: Form FileInfo
fileForm = renderDivs $ fileAFormReq "Anhang"

getEntriesR :: Text -> Handler RepHtml
getEntriesR catName = getEntriesByTagR catName []

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
  comments <- map (\ (e,c) -> (entityKey e, length c)) <$> (runDB $ runJoin (selectOneMany (CommentEntry <-.) commentEntry))
  entries <- if null tagNames
                then runDB $ selectList [EntryCat ==. (entityKey category)] [Desc EntryDate]
                else map fst <$> (runDB $ runJoin (selectOneMany (TaggedEntry <-.) taggedEntry)
                     { somFilterMany = [FilterOr . map (TaggedTag ==.) $ currentTags]
                     , somOrderOne = [Desc EntryDate]
                     })
  defaultLayout $ do
    if null tagNames
       then setTitle $ toHtml catName
       else setTitle $ toHtml $ catName `append` " :: " `append` (T.concat $ intersperse ", " tagNames)
    $(widgetFile "entries")

toggleTag :: Eq a => a -> [a] -> [a]
toggleTag t ts
  | t `elem` ts = L.delete t ts
  | otherwise = t:ts

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
  comments <- buildComments <$> (runDB $ selectList [CommentEntry ==. (entityKey entry)] [Asc CommentDate])
  ((_, formNew), _) <- runFormPost $ commentForm (Just $ PComment Nothing ((maybe Nothing (userName . entityVal)) mua) "") Nothing
  ((res, formEdit), enctype) <- runFormPost $ commentForm (Just $ PComment mparent ((maybe Nothing (userName . entityVal)) mua) "") mparent
  case res of
    FormSuccess p -> do
      now <- liftIO getCurrentTime
      _ <- runDB $ insert $ Comment (author p) (content p) now (parent p) (entityKey entry) False
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

getDeleteTagR :: Text -> TagId -> Handler ()
getDeleteTagR category tid = do
  requireAdmin
  runDB $ delete tid
  redirect $  EntriesR category

getNewEntryR :: Text -> Handler RepHtml
getNewEntryR catName = do
  requireAdmin
  category <- runDB $ getBy404 $ UniqueCategory catName
  tags <- return []
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
  runDB $ deleteWhere [CommentEntry ==. eid]
  runDB $ deleteWhere [TaggedEntry ==. eid]
  runDB $ deleteWhere [AttachmentEntry ==. eid]
  runDB $ delete eid
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
  atts <- runDB $ selectList [AttachmentEntry ==. (entityKey e)] [Asc AttachmentDescr]
  ((res, form), enctype) <- runFormPost $ fileForm
  case res of
       FormSuccess f -> do
          now <- liftIO $ getCurrentTime
          _ <- runDB $ insert $ Attachment (fileName f) (entityKey e) "" now
          liftIO $ BS.writeFile (buildFileName $ fileName f) (fileContent f)
          redirect $  EntryR catName curIdent
       _ -> return ()
  defaultLayout $
      $(widgetFile "upload-file")

postUploadFileR :: Text -> Text -> Handler RepHtml
postUploadFileR = getUploadFileR

getDeleteFileR :: Text -> Text -> AttachmentId -> Handler ()
getDeleteFileR catName curIdent aid = do
   requireAdmin
   a <- runDB $ get404 aid
   liftIO $ removeFile $ buildFileName $ attachmentFile a
   runDB $ delete aid
   redirect $  UploadFileR catName curIdent

-- Helper functions
buildFileName :: Text -> String
buildFileName name = Settings.staticDir ++ [pathSeparator] ++ unpack name

tagsForEntry :: Key backend (EntryGeneric backend) -> [(b, [Entity (TaggedGeneric backend)])] -> [b]
tagsForEntry eid = map fst . filter (any ((== eid) . taggedEntry . entityVal) . snd)

insertTags :: CategoryId -> EntryId -> [Text] -> Handler ()
insertTags category eid = mapM_ insertTag .filter (not . T.null)
           where insertTag t = do
                 mtag <- runDB $ getBy $ UniqueTag t category
                 tid <- (runDB $ insert $ Tag t category) -|- (entityKey <$> mtag)
                 runDB $ insert $ Tagged tid eid

buildComments :: [Entity (CommentGeneric b)] -> [(Integer, Entity (CommentGeneric b))]
buildComments cs = concat $ map flatten $ unfoldForest (\ c -> (c, getChilds c)) roots
      where
        roots = zip [0,0..] (filter (isNothing . commentParent . entityVal) cs)
        getChilds (depth, c) = zip (repeat $ succ depth) (filter (isChild c) cs)
        isChild (Entity c _) (Entity _ c') = maybe False (c ==) (commentParent c')

createStaticRoute :: Text -> StaticRoute
createStaticRoute name = StaticRoute [name] []

(-|-) :: Monad m => m a -> Maybe a -> m a
_ -|- Just a = return a
action -|- Nothing = action
