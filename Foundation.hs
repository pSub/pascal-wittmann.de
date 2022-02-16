{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foundation where

import Import.NoFoundation
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import           Yesod.Csp
import           Yesod.Csp.TH

-- Custom imports
import           Text.Blaze           (ToMarkup (..))
import           Text.Blaze.Internal  (preEscapedText)
import           Yesod.AtomFeed
-- import ILoveFS


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

type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

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

    urlParamRenderOverride _ NewsFeedR _ = Nothing
    urlParamRenderOverride _ (CommentFeedR _) _ = Nothing
    urlParamRenderOverride y r _ = Just $ uncurry (joinPath y "") $ renderRoute r

    -- Disable sessions for now due to DSGVO
    makeSessionBackend _ = return Nothing

    defaultLayout widget = do
        cspPolicy [csp|img-src 'self'; script-src 'none'; style-src 'self'; default-src 'none'|]
        master <- getYesod
        mmsg <- getMessage

        -- static links to images
        let profile_picture = StaticRoute ["profile.png"] []
        let netcup_oekostrom = StaticRoute ["netcup-oekostrom.png"] []

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            atomLink NewsFeedR "Newsfeed von pascal-wittmann.de"
            $(widgetFile "default-layout") -- >> ilovefs
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

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

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
        
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

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
