{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foundation where

import Import.NoFoundation
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth.OAuth2.Github
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

-- Custom imports
import           Text.Blaze           (ToMarkup (..))
import           Text.Blaze.Internal  (preEscapedText)
import           Yesod.AtomFeed

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appGithubOAuthKeys :: OAuthKeys
    }
    
instance HasHttpManager App where
         getHttpManager = appHttpManager
         
-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- TODO Swap this function into a utility module.
plural :: Int -> String -> String -> String
plural 1 x _ = x
plural _ _ y = y

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
         120 -- timeout in minutes
         "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuth
        cats <- runDB $ selectList [] [Asc CategoryName]
        currentRoute <- getCurrentRoute
        let isCurrent x = (currentRoute == Just x) || ((parents currentRoute) == Just x)
        let categories = map (name &&& EntriesR . name) cats

        -- static links to images
        let powered_by_logo = StaticRoute ["powered_by_yesod.png"] []
        let rss_logo = StaticRoute ["rss_logo.png"] []

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            atomLink NewsFeedR "Newsfeed von pascal-wittmann.de"
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
        where name = categoryName . entityVal

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR
    
    -- Routes not requiring authentication.
--    isAuthorized (AuthR _) _ = return Authorized
--    isAuthorized FaviconR _ = return Authorized
--    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
--    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
        
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

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
    authPlugins m = [ oauth2Github
                        (oauthKeysClientId $ appGithubOAuthKeys m)
                        (oauthKeysClientSecret $ appGithubOAuthKeys m)
                    ]

    authHttpManager = getHttpManager

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
      
unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger    

instance ToMarkup UTCTime where
  toMarkup = toMarkup . formatTime defaultTimeLocale "%e.%m.%Y"

instance ToMessage UTCTime where
  toMessage = toMessage . formatTime defaultTimeLocale "%e.%m.%Y"

parents :: Maybe (Route App) -> Maybe (Route App)
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

maybeAdmin :: Handler (Maybe User)
maybeAdmin = do
     mu <- maybeAuth
     case mu of
          Just (Entity _ u) -> do
            if userAdmin u
              then return $ Just u
              else return Nothing
          Nothing -> return Nothing
