module Handler.News
       ( getNewsFeedR
       ) where

import Import
import Data.Maybe
import Data.Time  (getCurrentTime)
import Data.List (find)
import Yesod.RST (rstToHtml)
import Yesod.Feed

getNewsFeedR :: Handler RepAtomRss
getNewsFeedR = do
    now <- liftIO getCurrentTime
    categories <- runDB $ selectList [] []
    entries <- runDB $ selectList [] [Desc EntryDate, LimitTo 30] >>= (mapM $ \(Entity _ e) -> return FeedEntry
            { feedEntryLink = EntryR (findCategory (entryCat e) categories) (entryIdent e)
            , feedEntryUpdated = entryDate e
            , feedEntryTitle = entryTitle e
            , feedEntryContent = rstToHtml $ entryContent e
            })
    newsFeed Feed
       { feedTitle = "pascal-wittmann.de"
       -- TODO add feedAuthor field
       , feedLinkSelf = NewsFeedR
       , feedLinkHome = RootR
       , feedDescription = "RSS/Atom Feed von pascal-wittmann.de"
       -- TODO change language to en
       , feedLanguage = "de"
       -- TODO use the appropriated date
       , feedUpdated = now
       , feedEntries = entries
       }
   where findCategory eid = categoryName . entityVal . fromJust . find ((eid ==) . entityKey)
