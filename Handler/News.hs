module Handler.News
       ( getNewsFeedR
       ) where

import Data.Maybe
import Data.Time  (getCurrentTime)
import Foundation
import Yesod.Feed

getNewsFeedR :: Handler RepAtomRss
getNewsFeedR = do
    now <- liftIO getCurrentTime
    categories <- runDB $ selectList [] []
    entries <- runDB $ selectList [] [Desc EntryDate, LimitTo 30] >>= (mapM $ \(_, e) -> return FeedEntry
            { feedEntryLink = EntryR (findCategory (entryCat e) categories) (entryIdent e)
            , feedEntryUpdated = entryDate e
            , feedEntryTitle = entryTitle e
            , feedEntryContent = toHtml $ entryRecap e
            })
    newsFeed Feed
       { feedTitle = "pascal-wittmann.de"
       , feedLinkSelf = NewsFeedR
       , feedLinkHome = RootR
       , feedDescription = "RSS/Atom Feed von pascal-wittmann.de"
       , feedLanguage = "de"
       , feedUpdated = now
       , feedEntries = entries
       }
   where findCategory eid = categoryName . fromJust . lookup eid
