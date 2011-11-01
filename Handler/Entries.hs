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

import           Foundation
import qualified Settings
import           Yesod.Goodies.Markdown
import           Database.Persist.Join     (selectOneMany, SelectOneMany(..))
import           Database.Persist.Join.Sql (runJoin)

import           Control.Applicative
import qualified Data.ByteString.Lazy      as BS (writeFile)
import           Data.List                 (intersperse, sort)
import qualified Data.List                 as L (delete)
import           Data.Maybe
import           Data.Text                 (Text, unpack, pack, append, strip, splitOn)
import qualified Data.Text                 as T
import           Data.Time
import           Data.Tree
import           Prelude                   hiding (unwords)
import           System.Directory
import           System.FilePath.Posix

data PEntry = PEntry
     { title :: Text
     , ident :: Text
     , cat :: CategoryId
     , tag :: Text
     , recap :: Text
     , text :: Markdown
     } deriving Show
                
data PComment = PComment
     { parent :: Maybe CommentId
     , author :: Maybe Text
     , content :: Markdown
     }
     
entryForm ::  Maybe PEntry -> CategoryId -> Html -> Form Homepage Homepage (FormResult PEntry, Widget)
entryForm mparams category html = (flip renderDivs) html $ PEntry
    <$> areq textField "Title" (title <$> mparams)
    <*> areq textField "Ident" (ident <$> mparams)
    <*> pure category
    <*> areq textField "Tags" (tag <$> mparams)
    <*> areq textField "Summary" (recap <$> mparams)
    <*> areq markdownField "Text" (text <$> mparams)

commentForm :: Maybe PComment -> Maybe CommentId -> Html -> Form Homepage Homepage (FormResult PComment, Widget)
commentForm mcomment mparent html = (flip renderDivs) html $ PComment
    <$> pure mparent
    <*> aopt textField "Name" (author <$> mcomment)
    <*> areq markdownField "Kommentar" (content <$> mcomment)

fileForm :: Html -> Form Homepage Homepage (FormResult FileInfo, Widget)
fileForm = renderDivs $ fileAFormReq "Anhang"

getEntriesR :: Text -> Handler RepHtml
getEntriesR catName = getEntriesByTagR catName []

getEntriesByTagR :: Text -> [Text] -> Handler RepHtml
getEntriesByTagR catName tagNames = do
  mu <- maybeAdmin
  category <- runDB $ getBy404 $ UniqueCategory catName
  currentTags <- mapMaybe (fst <$>) <$> mapM (\ n -> runDB $ getBy $ UniqueTag n (fst category)) tagNames
  tagsEntries <- runDB $ runJoin (selectOneMany (TaggedTag <-.) taggedTag)
          { somFilterOne = [TagCategory ==. (fst category)]
          , somOrderOne = [Asc TagName]
          }
  tags <- return $ map fst tagsEntries
  comments <- map (\ (e,c) -> (fst e, length c)) <$> (runDB $ runJoin (selectOneMany (CommentEntry <-.) commentEntry))
  entries <- if null tagNames
                then runDB $ selectList [EntryCat ==. (fst category)] [Desc EntryDate]
                else map fst <$> (runDB $ runJoin (selectOneMany (TaggedEntry <-.) taggedEntry)
                     { somFilterMany = [FilterOr . map (TaggedTag ==.) $ currentTags]
                     , somOrderOne = [Desc EntryDate]
                     })
  defaultLayout $ do
    setTitle $ toHtml $ catName `append` " :: " `append` (T.concat $ intersperse ", " tagNames)
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
          { somFilterMany = [TaggedEntry ==. (fst entry)] 
          , somOrderOne = [Asc TagName]
          }
  atts <- runDB $ selectList [AttachmentEntry ==. (fst entry)] [Asc AttachmentName]
  comments <- buildComments <$> (runDB $ selectList [CommentEntry ==. (fst entry)] [Asc CommentDate])
  ((_, formNew), _) <- runFormPost $ commentForm (Just $ PComment Nothing ((maybe Nothing (userName . snd)) mua) "") Nothing
  ((res, formEdit), enctype) <- runFormPost $ commentForm (Just $ PComment mparent ((maybe Nothing (userName . snd)) mua) "") mparent
  case res of
    FormSuccess p -> do
      now <- liftIO getCurrentTime
      _ <- runDB $ insert $ Comment (author p) (content p) now (parent p) (fst entry) False
      redirect RedirectTemporary $ EntryR catName curIdent
    _ -> return ()
  defaultLayout $ do
    setTitle $ toHtml $ entryTitle $ snd entry
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
  _ <- requireAdmin
  runDB $ delete tid
  redirect RedirectTemporary $ EntriesR category

getNewEntryR :: Text -> Handler RepHtml
getNewEntryR catName = do
  _ <- requireAdmin
  category <- runDB $ getBy404 $ UniqueCategory catName
  tags <- return []
  ((res, form), enctype) <- runFormPost $ entryForm Nothing (fst category)
  case res of
    FormSuccess p -> do
      now <- liftIO getCurrentTime
      aid <- runDB $ insert $ Entry (title p) (ident p) (text p) (cat p) (recap p) "" now
      insertTags (cat p) aid (buildTagList p)
      redirect RedirectTemporary $ EntriesR $ categoryName $ snd category
    _ -> return ()
  defaultLayout $ do
    setTitle "New Entry"
    $(widgetFile "new-entry")

postNewEntryR :: Text -> Handler RepHtml
postNewEntryR = getNewEntryR

getEditEntryR :: Text -> Text -> Handler RepHtml
getEditEntryR catName eid = do
  _ <- requireAdmin
  a <- runDB $ getBy404 $ UniqueEntry eid
  tags <- showTags <$> (runDB $ runJoin $ (selectOneMany (TaggedTag <-.) taggedTag)
          { somFilterMany = [TaggedEntry ==. (fst a)]
          , somOrderOne = [Asc TagName]
          })
  ((res, form), enctype) <- runFormPost $ entryForm (Just $ PEntry (entryTitle $ snd a) (entryIdent $ snd a) (entryCat $ snd a) tags (entryRecap $ snd a) (entryContent $ snd a)) (entryCat $ snd a)
  case res of
      FormSuccess p -> do
        category <- runDB $ get404 $ cat p
        runDB $ update (fst a) [EntryTitle =. (title p), EntryIdent =. (ident p), EntryContent =. (text p), EntryRecap =. (recap p)]
        runDB $ deleteWhere [TaggedEntry ==. (fst a)]
        insertTags (cat p) (fst a) (buildTagList p)
        redirect RedirectTemporary $ EntryR (categoryName category) (ident p)
      _ -> return ()
  defaultLayout $ do
       setTitle "Edit Entry"
       $(widgetFile "new-entry")
  where showTags = pack . concat . intersperse ", " . map (unpack . tagName . snd . fst)

buildTagList :: PEntry -> [Text]
buildTagList = map strip . splitOn "," . tag
    
postEditEntryR :: Text -> Text -> Handler RepHtml
postEditEntryR = getEditEntryR
      
getDeleteEntryR :: Text -> EntryId -> Handler ()
getDeleteEntryR category eid = do
  _ <- requireAdmin
  runDB $ deleteWhere [CommentEntry ==. eid]
  runDB $ deleteWhere [TaggedEntry ==. eid]
  runDB $ deleteWhere [AttachmentEntry ==. eid]
  runDB $ delete eid
  redirect RedirectTemporary $ EntriesR category
  
getDeleteCommentR :: Text -> Text -> CommentId -> Handler ()
getDeleteCommentR catName curIdent cid = do
  _ <- requireAdmin
  runDB $ update cid [CommentDeleted =. True]
  redirect RedirectTemporary $ EntryR catName curIdent

getUploadFileR :: Text -> Text -> Handler RepHtml
getUploadFileR catName curIdent = do
  _ <- requireAdmin
  e <- runDB $ getBy404 $ UniqueEntry curIdent
  atts <- runDB $ selectList [AttachmentEntry ==. (fst e)] [Asc AttachmentName]
  ((res, form), enctype) <- runFormPost fileForm
  case res of
       FormSuccess f -> do
          _ <- runDB $ insert $ Attachment (fileName f) (fst e)
          liftIO $ BS.writeFile (buildFileName $ fileName f) (fileContent f)
          redirect RedirectTemporary $ EntryR catName curIdent
       _ -> return ()
  defaultLayout $
      $(widgetFile "upload-file")

postUploadFileR :: Text -> Text -> Handler RepHtml
postUploadFileR = getUploadFileR

getDeleteFileR :: Text -> Text -> AttachmentId -> Handler ()
getDeleteFileR catName curIdent aid = do
   _ <- requireAdmin
   a <- runDB $ get404 aid
   liftIO $ removeFile $ buildFileName $ attachmentName a
   runDB $ delete aid
   redirect RedirectTemporary $ UploadFileR catName curIdent
  
-- Helper functions
buildFileName :: Text -> String
buildFileName name = Settings.staticDir ++ [pathSeparator] ++ unpack name

tagsForEntry :: Key backend Entry -> [(b, [(a, TaggedGeneric backend)])] -> [b]
tagsForEntry eid = map fst . filter (any ((== eid) . taggedEntry . snd) . snd)

insertTags :: CategoryId -> EntryId -> [Text] -> Handler ()
insertTags category eid = mapM_ insertTag .filter (not . T.null)
           where insertTag t = do
                 mtag <- runDB $ getBy $ UniqueTag t category
                 tid <- (-|-) (runDB $ insert $ Tag t category) (fst <$> mtag)
                 runDB $ insert $ Tagged tid eid

buildComments :: [(Key backend Comment, CommentGeneric backend)] ->
                [(Integer, (Key backend Comment, CommentGeneric backend))]
buildComments cs = concat $ map flatten $ unfoldForest (\ c -> (c, getChilds c)) roots
      where
        roots = zip [0,0..] (filter (isNothing . commentParent . snd) cs)
        getChilds (cid, c) = zip (repeat $ succ cid) (filter (isChild c) cs)
        isChild (c,_) (_,c') = (isJust $ commentParent c') && c == (fromJust $ commentParent c')

createStaticRoute :: Text -> StaticRoute
createStaticRoute name = StaticRoute [name] []

(-|-) :: Monad m => m a -> Maybe a -> m a
_ -|- Just a = return a
action -|- Nothing = action
