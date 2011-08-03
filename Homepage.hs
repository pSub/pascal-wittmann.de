{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies,
             QuasiQuotes,
             TemplateHaskell,
             OverloadedStrings,
             MultiParamTypeClasses #-}

module Homepage where

import Yesod
import Yesod.Form
import Yesod.Auth
import Yesod.Auth.Email
import Yesod.Goodies.Markdown
import Network.Mail.Mime
import Data.Maybe (isJust)
import Data.Time.Clock
import Control.Monad (join, unless)
import Text.Blaze
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Settings
import Model
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
                    /linux.html LinuxR GET
                    /log.html LogR GET
                    /newlog.html NewLogR GET POST
                    /editlog/#ArticleId EditLogR GET POST
                    /referate.html ReferateR GET
                    /login.html AuthR Auth getAuth
                                   |]
  
-- Sections displayed in menu
section :: [(String, HomepageRoute)]
section = 
  [ ( "Linux", LinuxR )
  , ( "Log"  , LogR )
  , ( "Referate", ReferateR)
  ]

instance Yesod Homepage where
  approot _ =  Settings.approot
  
  defaultLayout widget = do
    mu <- maybeAuth
    current <- getCurrentRoute
    toMaster <- getRouteToMaster
    (title, parents) <- breadcrumbs
    let isCurrent x = fmap toMaster current == Just x ||
                      x `elem` map fst parents
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
  breadcrumb (AuthR _) = return("Login", Nothing)
  breadcrumb NewLogR = return ("Neuer Eintrag", Nothing)
  breadcrumb (EditLogR _) = return ("Eintrag bearbeiten", Nothing)
  breadcrumb _     = return ("404", Nothing)
  
instance YesodAuth Homepage where
  type AuthId Homepage = UserId
  
  -- Where to send a user after successful login
  loginDest _ = RootR
  
  -- Where to send a user after logout
  logoutDest _ = RootR
          
  getAuthId creds = runDB $ do
    x <- getBy $ UniqueUser $ credsIdent creds
    case x of
      Just (uid, _) -> return $ Just uid
      Nothing -> do
        fmap Just $ insert $ User (credsIdent creds) Nothing
        
  authPlugins = [authEmail]

instance RenderMessage Homepage FormMessage where
  renderMessage _ _ = defaultFormMessage
  
instance YesodAuthEmail Homepage where
    type AuthEmailId Homepage = EmailId

    addUnverified email verkey =
        runDB $ insert $ Email email Nothing $ Just verkey
    sendVerifyEmail email _ verurl = liftIO $ renderSendMail Mail
        { mailHeaders =
            [ ("From", "noreply")
            , ("To", email)
            , ("Subject", "Verify your email address")
            ]
        , mailParts = [[textPart, htmlPart]]
        }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8
                          $ Data.Text.Lazy.unlines
                [ "Please confirm your email address by clicking on the link below."
                , ""
                , Data.Text.Lazy.fromChunks [verurl]
                , ""
                , "Thank you"
                ]
            , partHeaders = []
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml [hamlet|
<p>Please confirm your email address by clicking on the link below.
<p>
    <a href=#{verurl}>#{verurl}
<p>Thank you
|]
            , partHeaders = []
            }
    getVerifyKey = runDB . fmap (join . fmap emailVerkey) . get
    setVerifyKey eid key = runDB $ update eid [EmailVerkey $ Just key]
    verifyAccount eid = runDB $ do
        me <- get eid
        case me of
            Nothing -> return Nothing
            Just e -> do
                let email = emailEmail e
                case emailUser e of
                    Just uid -> return $ Just uid
                    Nothing -> do
                        uid <- insert $ User email Nothing
                        update eid [EmailUser $ Just uid, EmailVerkey Nothing]
                        return $ Just uid
    getPassword = runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = runDB $ update uid [UserPassword $ Just pass]
    getEmailCreds email = runDB $ do
        me <- getBy $ UniqueEmail email
        case me of
            Nothing -> return Nothing
            Just (eid, e) -> return $ Just EmailCreds
                { emailCredsId = eid
                , emailCredsAuthId = emailUser e
                , emailCredsStatus = isJust $ emailUser e
                , emailCredsVerkey = emailVerkey e
                }
    getEmail = runDB . fmap (fmap emailEmail) . get

-- How to run database actions.
instance YesodPersist Homepage where
    type YesodDB Homepage = SqlPersist
    runDB db = liftIOHandler $ fmap connPool getYesod >>= Settings.runConnectionPool db

instance ToHtml Markdown where
  toHtml (Markdown s) = toHtml s

instance ToHtml UTCTime where
  toHtml = toHtml . show
