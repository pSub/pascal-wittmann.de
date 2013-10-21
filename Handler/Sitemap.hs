module Handler.Sitemap
       ( getSitemapR
       ) where

import           Data.List     (find)
import           Data.Maybe    (fromJust)
import           Import
import           Yesod.Sitemap

getSitemapR :: Handler TypedContent
getSitemapR = do
    categories <- runDB $ select $ from return
    entries <- runDB $ select $ from return
    categories_urls <- return $ map (\(Entity _ v) -> SitemapUrl
                    { sitemapLoc = EntriesR $ categoryName v
                    , sitemapLastMod = lastChange entries
                    , sitemapChangeFreq = Just $ Weekly
                    , sitemapPriority = Just 0.7
                    }) categories
    entries_urls <- return $ map (\(Entity _ v) -> SitemapUrl
            { sitemapLoc = EntryR (findCategory (entryCat v) categories) $ entryIdent v
            , sitemapLastMod = Just $ entryLastMod v
            , sitemapChangeFreq = Just $ Monthly
            , sitemapPriority = Just 0.9
            }) entries
    statics <- (runDB $ select $ from return) >>= return . map(\(Entity _ v) -> SitemapUrl
            { sitemapLoc = StaticR $ StaticRoute [attachmentFile v] []
            , sitemapLastMod = Just $ attachmentLastMod v
            , sitemapChangeFreq = Just $ Never
            , sitemapPriority = Just 0.8
            })
    sitemapList $ [ SitemapUrl RootR (lastChange entries) (Just Monthly) (Just 1.0)
                -- TODO: This approximation is still rather bad
              , SitemapUrl ImpressumR (lastChange entries) (Just Yearly) (Just 0.2)
              ] ++ categories_urls ++ entries_urls ++ statics
   where findCategory eid = categoryName . entityVal . fromJust . find ((eid ==) . entityKey)
         lastChange = Just . maximum . map (entryLastMod . entityVal)
