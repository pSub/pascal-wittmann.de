module Handler.Sitemap
       ( getSitemapR
       ) where

import           Data.List       (find)
import           Data.Maybe      (fromJust)
import           Data.Time.Clock
import           Import
import           Yesod.Sitemap

getSitemapR :: Handler RepXml
getSitemapR = do
    categories <- runDB $ selectList [] []
    entries <- runDB $ selectList [] []
    categories_urls <- return $ map (\(Entity _ v) -> SitemapUrl
                    { sitemapLoc = EntriesR $ categoryName v
                    , sitemapLastMod = lastChange entries
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
            , sitemapLastMod = attachmentLastMod v
            , sitemapChangeFreq = Never
            , sitemapPriority = 0.8
            })
    sitemap $ [ SitemapUrl RootR (lastChange entries) Monthly 1.0
                -- TODO: This approximation is still rather bad
              , SitemapUrl ImpressumR (lastChange entries) Yearly 0.2
              ] ++ categories_urls ++ entries_urls ++ statics
   where findCategory eid = categoryName . entityVal . fromJust . find ((eid ==) . entityKey)
         lastChange = maximum . map (entryLastMod . entityVal)
