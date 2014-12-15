{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import Import (widgetFile)
data Layout = Layout

mkYesod "Layout" [parseRoutes|
/ HomeR GET
|]

instance Yesod Layout


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
	setTitle "Basic Layout"
	$(widgetFile "layout")


main :: IO ()
main = warp 3000 Layout