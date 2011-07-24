{-# LANGUAGE OverloadedStrings #-}

module Handlers
       ( getFaviconR
       , getRobotsR
       , module X
       ) where 
  
import Yesod
import Homepage
import Model

import Handlers.Root as X
import Handlers.Impressum as X
import Handlers.Linux as X
import Handlers.Referate as X
import Handlers.Log as X
import Handlers.NewLog as X

getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: String)
