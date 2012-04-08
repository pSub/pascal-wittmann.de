module Handler.Sitemap
       ( getSitemapR
       ) where

import Import
import Data.Time.Clock
import Data.Maybe (fromJust)
import Data.List (find)
import Yesod.Sitemap

getSitemapR :: Handler RepXml
getSitemapR = do
    time <- liftIO getCurrentTime
    categories <- runDB $ selectList [] []
    entries <- runDB $ selectList [] []
    categories_urls <- return $ map (\(Entity _ v) -> SitemapUrl
                    { sitemapLoc = EntriesR $ categoryName v
                    , sitemapLastMod = maximum $ map (entryLastMod . entityVal) entries
                    , sitemapChangeFreq = Weekly
                    , sitemapPriority = 0.7
                    }) categories
    entries_urls <- return $ map (\(Entity _ v) -> SitemapUrl
            { sitemapLoc = EntryR (findCategory (entryCat v) categories) $ entryIdent v
            , sitemapLastMod = entryLastMod v
            , sitemapChangeFreq = Monthly
            , sitemapPriority = 0.9
            }) entries
    statics <- runDB $ selectList [] [] >>= return . map(\(Entity _ v) -> SitemapUrl
            { sitemapLoc = StaticR $ StaticRoute [attachmentFile v] []
            , sitemapLastMod = time
            , sitemapChangeFreq = Never
            , sitemapPriority = 0.8
            })
    sitemap $ [ SitemapUrl RootR time Monthly 1.0
              , SitemapUrl ImpressumR time Yearly 0.2
              ] ++ categories_urls ++ entries_urls ++ statics
   where findCategory eid = categoryName . entityVal . fromJust . find ((eid ==) . entityKey)
