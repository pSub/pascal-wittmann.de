{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies,
             QuasiQuotes,
             TemplateHaskell,
             OverloadedStrings #-}

module Homepage where

import Yesod
import qualified Settings

--import Text.Hamlet
import Text.Hamlet.NonPoly (IHamlet, ihamletFile)
import Text.Cassius

data Homepage = Homepage

type Handler = GHandler Homepage Homepage
type Widget = GWidget Homepage Homepage

mkYesodData "Homepage" [parseRoutes|
                    / RootR GET
                    /robots.txt RobotsR GET
                    /impressum.html ImpressumR GET
                   |]

instance Yesod Homepage where
  approot _ =  Settings.approot
  defaultLayout widget = do
    (title, bcs) <- breadcrumbs
    pc <- widgetToPageContent $ do
      setTitleI title
      addCassius $(Settings.cassiusFile "default-layout")
      addWidget widget
    hamletToRepHtml $(Settings.hamletFile "default-layout")
    
instance YesodBreadcrumbs Homepage where
  breadcrumb RootR = return ("Home", Nothing)
  breadcrumb ImpressumR = return ("Impressum", Nothing)
  breadcrumb _     = return ("404", Nothing)
