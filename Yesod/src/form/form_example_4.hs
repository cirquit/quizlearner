{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Control.Applicative
import           Data.Text           (Text)
import           Data.Time
import           Yesod

-- In the authentication chapter, we'll address this properly
newtype UserId = UserId Int
    deriving Show

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod App

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

type Form a = Html -> MForm Handler (FormResult a, Widget)

data Blog = Blog
    { blogTitle    :: Text
    , blogContents :: Textarea
    , blogUser     :: UserId
    , blogPosted   :: UTCTime
    }
    deriving Show

form :: UserId -> Form Blog
form userId = renderDivs $ Blog
    <$> areq textField "Title" Nothing
    <*> areq textareaField "Contents" Nothing
    <*> pure userId
    <*> lift (liftIO getCurrentTime)

getHomeR :: Handler Html
getHomeR = do
    let userId = UserId 5 -- again, see the authentication chapter
    ((res, widget), enctype) <- runFormPost $ form userId
    defaultLayout $ do
        [whamlet|
            <p>Previous result: #{show res}
            <form method=post action=@{HomeR} enctype=#{enctype}>
                ^{widget}
                <input type=submit>
        |]

postHomeR :: Handler Html
postHomeR = getHomeR

main :: IO ()
main = warp 3000 App