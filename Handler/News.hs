module Handler.News
       ( getNewsFeedR
       , getCommentFeedR
       ) where

import           Data.List as L      (maximum)
import           Data.Maybe
import           Import
import           Yesod.Markdown (markdownToHtml)

getNewsFeedR :: Handler TypedContent
getNewsFeedR = do
    entries <- runDB $ select $ from $ \e -> do
                        orderBy [desc (e ^. EntryDate)]
                        limit 30
                        return e
    feeds <- mapM (\(Entity _ e) -> return FeedEntry
                   { feedEntryLink = EntryR $ entryIdent e
                   , feedEntryUpdated = entryDate e
                   , feedEntryTitle = entryTitle e
                   , feedEntryContent = either (const "Error") id $ markdownToHtml $ entryContent e
                   , feedEntryEnclosure = Nothing
                   , feedEntryCategories = []
                   }) entries
    newsFeed Feed
       { feedTitle = "pascal-wittmann.de"
       , feedAuthor = "Pascal Wittmann"
       , feedLinkSelf = NewsFeedR
       , feedLinkHome = EntriesR
       , feedDescription = "RSS/Atom Feed von pascal-wittmann.de"
       , feedLanguage = "en"
       , feedUpdated = L.maximum $ map feedEntryUpdated feeds
       , feedLogo = Nothing
       , feedEntries = feeds
       }

getCommentFeedR :: Key Entry -> Handler TypedContent
getCommentFeedR key = do
    e <- runDB $ get404 key
    comments <- (runDB $ select $ from $ \c -> do
                        where_ (c ^. CommentEntry ==. val key)
                        orderBy [asc (c ^. CommentDate)]
                        return c) >>= (mapM $ \(Entity _ c) -> return FeedEntry
             { feedEntryLink = EntryR $ entryIdent e
             , feedEntryUpdated = commentDate c
             , feedEntryTitle = (entryTitle e) `mappend` " -- " `mappend` (fromMaybe "anonymous" (commentAuthor c))
             , feedEntryContent = either (const "Error") id $ markdownToHtml $ commentContent c
             , feedEntryEnclosure = Nothing
             , feedEntryCategories = []
             })
    newsFeed Feed
       { feedTitle = "Comments for entry '" `mappend` (entryTitle e) `mappend` "' on pascal-wittmann.de"
       , feedAuthor = "…"
       , feedLinkSelf = CommentFeedR key
       , feedLinkHome = EntriesR
       , feedDescription = "This feed contains all comments for this entry."
       , feedLanguage = "en"
       , feedUpdated = L.maximum $ map feedEntryUpdated comments
       , feedLogo = Nothing
       , feedEntries = comments
       }
