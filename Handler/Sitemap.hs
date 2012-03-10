module Handler.Sitemap
       ( getSitemapR
       ) where

import Import

getSitemapR :: GHandler s m RepXml
getSitemapR = sendFile "application/xml" "config/sitemap.xml"
