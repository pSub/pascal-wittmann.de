{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Settings
       ( approot
       , hamletFile
       , cassiusFile
       ) where

import qualified Text.Hamlet
import qualified Text.Cassius
import qualified Data.Text as T

approot :: T.Text
approot = "http://localhost:3000"

hamletFile x  = Text.Hamlet.hamletFile  $ "hamlet/"  ++ x ++ ".hamlet"
cassiusFile x = Text.Cassius.cassiusFile $ "cassius/" ++ x ++ ".cassius"
