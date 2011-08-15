{-# LANGUAGE OverloadedStrings #-}

module Handlers
       ( getFaviconR
       , getRobotsR
       , module X
       ) where 
  
import Yesod
import Homepage

import Handlers.Root as X
import Handlers.Impressum as X
import Handlers.Articles as X
import Handlers.Admin as X

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: String)
