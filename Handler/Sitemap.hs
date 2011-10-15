{-# LANGUAGE OverloadedStrings #-}

module Handler.Sitemap
       ( getSitemapR
       ) where

import Foundation

getSitemapR :: GHandler s m RepXml
getSitemapR = sendFile "application/xml" "config/sitemap.xml"
