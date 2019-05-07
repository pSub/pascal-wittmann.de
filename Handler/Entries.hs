module Handler.Entries
       ( getEntriesR
       , getEntriesByTagR
       , getEntryR
       , postEntryR
       , getEntryCommentR
       , postEntryCommentR
       ) where

import qualified Data.List             as L (delete, intersperse)
import           Data.Maybe
import qualified Data.Text             as T
import           Data.Time
import           Data.Tree
import           Import                hiding (isNothing)
import           Text.Regex
import           Yesod.Markdown

-- Simple regex to match urls
urlRegex :: Regex
urlRegex = mkRegex "\\b(https?|ftp|file)://[-A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]"

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
        noURLsMarkdownField = checkBool (\t -> loggedin || isNothing (matchRegex urlRegex (unpack $ unMarkdown t)))
                                    ("URLs are not allowed." :: Text)
                                    markdownField

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
  category <- runDB $ getBy404 $ UniqueCategory catName
  currentTags <- mapMaybe (entityKey <$>) <$> mapM (\ n -> runDB $ getBy $ UniqueTag n (entityKey category)) tagNames
  tagging <- runDB $ select $ from $ \(t `InnerJoin` s) -> do
                     on $ t ^. TagId ==. s ^. TaggedTag
                     orderBy [asc (t ^. TagName)]
                     return (t, s)

  tags <- runDB $ select $ from $ \t -> where_ (t ^. TagCategory ==. val (entityKey category)) >> return t
  comments <- map (\(Value e, Value c) -> (e, c)) <$> runDB (select $ from $ \(e `InnerJoin` c) -> do
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
       else setTitle $ toHtml $ catName <> " :: " <> T.concat (L.intersperse ", " tagNames)
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
  entry <- runDB $ getBy404 $ UniqueEntry curIdent
  atts <- runDB $ select $ from $ \a -> do
                  where_ (a ^. AttachmentEntry ==. val (entityKey entry))
                  orderBy [asc (a ^. AttachmentDescr)]
                  return a
  comments <- buildComments <$> runDB (select $ from $ \c -> do
                                         where_ (c ^. CommentEntry ==. val (entityKey entry))
                                         where_ (c ^. CommentDeleted ==. val False)
                                         orderBy [asc (c ^. CommentDate)]
                                         return c)
  now <- liftIO getCurrentTime
  ((_, formNew), _) <- runFormPost $ commentForm False Nothing "" now Nothing (entityKey entry)
  ((res, formEdit), enctype) <- runFormPost $ commentForm False Nothing "" now mparent (entityKey entry)
  case res of
    FormSuccess comment -> do
      _ <- runDB $ insert comment
      redirect $ EntryR catName curIdent
    _ -> return ()
  defaultLayout $ do
    setTitle $ toHtml $ entryTitle $ entityVal entry
    $(widgetFile "entry")

computeIndentionClass :: Integer -> [(String, String)]
computeIndentionClass indention = [("class", "indent-" ++ (show $ (2 * indention) `mod` 10) ++ " comment")]

getEntryCommentR :: Text -> Text -> CommentId -> Handler Html
getEntryCommentR catName curIdent curParent = entryHandler catName curIdent (Just curParent)

postEntryCommentR :: Text -> Text -> CommentId -> Handler Html
postEntryCommentR = getEntryCommentR

getEntryR :: Text -> Text ->  Handler Html
getEntryR catName curIdent = entryHandler catName curIdent Nothing
         
postEntryR :: Text -> Text -> Handler Html
postEntryR = getEntryR

tagsForEntry :: Key Entry -> [(b, Entity Tagged)] -> [b]
tagsForEntry eid = map fst . filter (((== eid) . taggedEntry . entityVal) . snd)

buildComments :: [Entity Comment] -> [(Integer, Entity Comment)]
buildComments cs = concatMap flatten $ unfoldForest (id &&& getChilds) roots
      where
        roots = zip [0,0..] (filter (isNothing . commentParent . entityVal) cs)
        getChilds (depth, c) = zip (repeat $ succ depth) (filter (isChild c) cs)
        isChild (Entity key _) (Entity _ value) = maybe False (key ==) (commentParent value)

createStaticRoute :: Text -> StaticRoute
createStaticRoute name = StaticRoute [name] []
