-- 0.10 compatible
module Foundation
    ( Homepage (..)
    , Route (..)
    , HomepageMessage (..)
    , resourcesHomepage
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    , StaticRoute
    , AuthRoute
    , requireAdmin
    , maybeAdmin
    ) where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Logger (Logger, logMsg, formatLogText)
import Network.HTTP.Conduit (Manager)
#ifdef DEVELOPMENT
import Yesod.Logger (logLazyText)
#endif
import qualified Settings
import qualified Data.ByteString.Lazy as L
import qualified Database.Persist.Store
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
#if DEVELOPMENT
import qualified Data.Text.Lazy.Encoding
#else
import Network.Mail.Mime (sendmail)
#endif

-- Custom imports
import Control.Applicative
import Data.Text (Text)
import Data.Time
import Text.Blaze (ToHtml)
import System.Locale
import Yesod.Markdown
import Yesod.AtomFeed
import Settings.StaticFiles

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Homepage = Homepage
    { settings :: AppConfig DefaultEnv Extra
    , getLogger :: Logger
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

-- Set up i18n messages. See the message folder.
mkMessage "Homepage" "messages" "en"

-- This is only temporary, since complex types are
-- currently not allowed in routes
type Tags = [Text]

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype HomepageRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Homepage = HomepageRoute
-- * Creates the value resourcesHomepage which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Homepage. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the HomepageRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Homepage" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Homepage Homepage (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Homepage where
    approot = ApprootMaster $ appRoot . settings

    -- Place the session key file in the config folder
    encryptKey _ = fmap Just $ getKey "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        mu <- maybeAuth
        current <- getCurrentRoute
        toMaster <- getRouteToMaster
        cats <- runDB $ selectList [] [Asc CategoryName]
        let currentRoute = toMaster <$> current
        let isCurrent x = (currentRoute == Just x) || ((parents currentRoute) == Just x)
        let categories = map (\c -> (name c, EntriesR $ name c)) cats
        let powered_by_logo = StaticRoute ["powered_by_yesod.png"] []

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            atomLink NewsFeedR "Newsfeed von pascal-wittmann.de"
            $(widgetFile "normalize")
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")
        where name = categoryName . entityVal

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Enable Javascript async loading
    yepnopeJs _ = Just $ Right $ StaticR js_modernizr_js

-- How to run database actions.
instance YesodPersist Homepage where
    type YesodPersistBackend Homepage = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth Homepage where
    type AuthId Homepage = UserId

    -- Where to send a user after successful login
    loginDest _ = RootR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing Nothing False

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authGoogleEmail]

    authHttpManager = httpManager

-- Sends off your mail. Requires sendmail in production!
deliver :: Homepage -> L.ByteString -> IO ()
#ifdef DEVELOPMENT
deliver y = logLazyText (getLogger y) . Data.Text.Lazy.Encoding.decodeUtf8
#else
deliver _ = sendmail
#endif

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Homepage FormMessage where
    renderMessage _ _ = defaultFormMessage

instance ToHtml Markdown where
  toHtml (Markdown s) = toHtml s

instance ToHtml UTCTime where
  toHtml = toHtml . formatTime defaultTimeLocale "%e.%m.%Y"

parents :: Maybe (Route Homepage) -> Maybe (Route Homepage)
parents (Just ImpressumR) = Nothing
parents (Just (EntriesByTagR cat _)) = Just $ EntriesR cat
parents (Just (EntryR cat _)) = Just $ EntriesR cat
parents (Just (NewEntryR cat)) = Just $ EntriesR cat
parents (Just (UploadFileR cat _)) = Just $ EntriesR cat
parents (Just (EntryCommentR cat _ _)) = Just $ EntriesR cat
parents (Just (EditEntryR cat _)) = Just $ EntriesR cat
parents (Just _) = Nothing
parents Nothing = Nothing

requireAdmin :: Handler ()
requireAdmin = do
     (Entity _ u) <- requireAuth
     if userAdmin u
       then return ()
       else permissionDenied "You need admin privileges to do that"

maybeAdmin :: Handler (Maybe (UserGeneric SqlPersist))
maybeAdmin = do
     mu <- maybeAuth
     case mu of
          Just (Entity _ u) -> do
            if userAdmin u
              then return $ Just u
              else return Nothing
          Nothing -> return Nothing
