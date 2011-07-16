{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies,
             QuasiQuotes,
             TemplateHaskell,
             OverloadedStrings #-}

module Homepage where

import Yesod
import qualified Settings
import Model

--import Text.Hamlet
import Text.Hamlet.NonPoly (IHamlet, ihamletFile)
import Text.Cassius
import Database.Persist.GenericSql

data Homepage = Homepage
                {
                  connPool :: Settings.ConnectionPool 
                }

type Handler = GHandler Homepage Homepage
type Widget = GWidget Homepage Homepage

mkYesodData "Homepage" [parseRoutes|
                    / RootR GET
                    /robots.txt RobotsR GET
                    /impressum.html ImpressumR GET
                    /linux.hmtml LinuxR GET
                    /log.html LogR GET
                    /referate.html ReferateR GET
                                   |]

instance Yesod Homepage where
  approot _ =  Settings.approot
  defaultLayout widget = do
    (title, bcs) <- breadcrumbs
    pc <- widgetToPageContent $ do
      addCassius $(Settings.cassiusFile "default-layout")
      addWidget widget
    hamletToRepHtml $(Settings.hamletFile "default-layout")
    
instance YesodBreadcrumbs Homepage where
  breadcrumb RootR = return ("Home", Nothing)
  breadcrumb ImpressumR = return ("Impressum", Nothing)
  breadcrumb LinuxR = return ("Linux", Nothing)
  breadcrumb LogR = return ("Log", Nothing)
  breadcrumb ReferateR = return ("Referate", Nothing)
  breadcrumb _     = return ("404", Nothing)
  

-- How to run database actions.
instance YesodPersist Homepage where
    type YesodDB Homepage = SqlPersist
    runDB db = liftIOHandler $ fmap connPool getYesod >>= Settings.runConnectionPool db

section :: [(String, HomepageRoute)]
section = 
  [ ( "Linux", LinuxR )
  , ( "Log"  , LogR )
  , ( "Referate", ReferateR)
  ]
