module Handler.News
       ( getNewsFeedR
       , getCommentFeedR
       ) where

import           Data.List      (find)
import           Data.Maybe
import           Import
import           Yesod.AtomFeed
import           Yesod.Feed
import           Yesod.Markdown (markdownToHtml)

getNewsFeedR :: Handler TypedContent
getNewsFeedR = do
    categories <- runDB $ select $ from $ \c -> return c
    entries <- runDB $ select $ from $ \e -> do
                        orderBy [desc (e ^. EntryDate)]
                        limit 30
                        return e
    feeds <- mapM (\(Entity _ e) -> return FeedEntry
                   { feedEntryLink = EntryR (findCategory (entryCat e) categories) (entryIdent e)
                   , feedEntryUpdated = entryDate e
                   , feedEntryTitle = entryTitle e
                   , feedEntryContent = markdownToHtml $ entryContent e
                   }) entries
    newsFeed Feed
       { feedTitle = "pascal-wittmann.de"
       , feedAuthor = "Pascal Wittmann"
       , feedLinkSelf = NewsFeedR
       , feedLinkHome = RootR
       , feedDescription = "RSS/Atom Feed von pascal-wittmann.de"
       , feedLanguage = "en"
       , feedUpdated = maximum $ map feedEntryUpdated feeds
       , feedEntries = feeds
       }

getCommentFeedR :: Key Entry -> Handler TypedContent
getCommentFeedR key = do
    e <- runDB $ get404 key
    categories <- runDB $ select $ from $ \c -> return c
    comments <- (runDB $ select $ from $ \c -> do
                        where_ (c ^. CommentEntry ==. val key)
                        orderBy [asc (c ^. CommentDate)]
                        return c) >>= (mapM $ \(Entity _ c) -> return FeedEntry
             { feedEntryLink = EntryR (findCategory (entryCat e) categories) (entryIdent e)
             , feedEntryUpdated = commentDate c
             , feedEntryTitle = (entryTitle e) `mappend` " -- " `mappend` (fromMaybe "anonymous" (commentAuthor c))
             , feedEntryContent = markdownToHtml $ commentContent c
             })
    newsFeed Feed
       { feedTitle = "Comments for entry '" `mappend` (entryTitle e) `mappend` "' on pascal-wittmann.de"
       , feedAuthor = "…"
       , feedLinkSelf = CommentFeedR key
       , feedLinkHome = RootR
       , feedDescription = "This feed contains all comments for this entry."
       , feedLanguage = "en"
       , feedUpdated = maximum $ map feedEntryUpdated comments
       , feedEntries = comments
       }

findCategory :: Key Category -> [Entity Category] -> Text
findCategory eid = categoryName . entityVal . fromJust . find ((eid ==) . entityKey)
