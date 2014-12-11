{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import Import (widgetFile)
data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

test1 :: String 
test1 = "JOHNY"

test2 :: String 
test2 = "GILBERT"

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
	$(widgetFile "helloworld")


main :: IO ()
main = warp 3000 HelloWorld

