module Handler.Sitemap
       ( getSitemapR
       ) where

import qualified Data.List as L
import           Import
import           Yesod.Sitemap

getSitemapR :: Handler TypedContent
getSitemapR = do
    entries <- runDB $ select $ from return
    entries_urls <- return $ map (\(Entity _ v) -> SitemapUrl
            { sitemapLoc = EntryR $ entryIdent v
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
    sitemapList $ [ SitemapUrl EntriesR (lastChange entries) (Just Monthly) (Just 1.0)
                -- TODO: This approximation is still rather bad
              , SitemapUrl PrivacyR (lastChange entries) (Just Yearly) (Just 0.2)
              , SitemapUrl AboutR (lastChange entries) (Just Yearly) (Just 0.2)
              , SitemapUrl ContactR (lastChange entries) (Just Yearly) (Just 0.2)
              ] ++ entries_urls ++ statics
   where lastChange = Just . L.maximum . map (entryLastMod . entityVal)
