module Foundation where

import ClassyPrelude.Yesod
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Model
import Settings
import Settings.StaticFiles
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Auth
import Yesod.Auth.BrowserId (authBrowserId)
import Yesod.Core.Types     (Logger)
import Yesod.Default.Util   (addStaticContentExternal)
import Web.Cookie                         (SetCookie (..))
import qualified Data.Text as T
import Web.Authenticate.BrowserId

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


---- | i18n support

mkMessage "App" "messages" "en"

plural :: Int -> String -> String -> String
plural 1 x _ = x
plural _ _ y = y

-- | SSL Security copied from Yesod.Core because the
--   the current core version 1.4.6 is depricated

-- | Defends against session hijacking by setting the secure bit on session
-- cookies so that browsers will not transmit them over http. With this
-- setting on, it follows that the server will regard requests made over
-- http as sessionless, because the session cookie will not be included in
-- the request. Use this as part of a total security measure which also
-- includes disabling HTTP traffic to the site or issuing redirects from
-- HTTP urls, and composing 'sslOnlyMiddleware' with the site's
-- 'yesodMiddleware'.
--
-- Since 1.4.7
sslOnlySessions :: IO (Maybe SessionBackend) -> IO (Maybe SessionBackend)
sslOnlySessions = (fmap . fmap) secureSessionCookies
  where
    setSecureBit cookie = cookie { setCookieSecure = True }
    secureSessionCookies = customizeSessionCookies setSecureBit

-- | Apply a Strict-Transport-Security header with the specified timeout to
-- all responses so that browsers will rewrite all http links to https
-- until the timeout expires. For security, the max-age of the STS header
-- should always equal or exceed the client sessions timeout. This defends
-- against hijacking attacks on the sessions of users who attempt to access
-- the site using an http url. This middleware makes a site functionally
-- inaccessible over vanilla http in all standard browsers.
--
-- Since 1.4.7
sslOnlyMiddleware :: Yesod site
                     => Int -- ^ minutes
                     -> HandlerT site IO res
                     -> HandlerT site IO res
sslOnlyMiddleware timeout handler = do
    addHeader "Strict-Transport-Security"
              $ T.pack $ concat [ "max-age="
                                , show $ timeout * 60
                                , "; includeSubDomains"
                                ]
    handler


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

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    -- sslOnlySessions stores all session cookies with the secure bit on,
    -- so they will not be transmitted over http
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

   -- makeSessionBackend _ = sslOnlySessions $
   --    fmap Just $ defaultClientSessionBackend 120 "config/client_session_key.aes"
    yesodMiddleware = (sslOnlyMiddleware 120) . defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            addScript $ StaticR js_jQuery_v1_11_2_js
            addScriptRemote browserIdJs
            addScript $ StaticR js_getVerifiedEmail_js
            setTitle "Quizlearner"
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

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

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

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
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert User
                    { userIdent = credsIdent creds
                    , userPassword = Nothing
                    }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId def]

    authHttpManager = getHttpManager

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding