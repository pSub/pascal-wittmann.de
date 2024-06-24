{-# LANGUAGE OverloadedStrings #-}
module Handler.CachedFavicon
    ( getCachedFaviconR
    ) where

import Control.Monad ((>>))
import Yesod.Core

getCachedFaviconR :: MonadHandler m => m ()
getCachedFaviconR = cacheSeconds 691200 >> sendFile "image/x-icon" "config/favicon.ico"
