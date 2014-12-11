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
<<<<<<< HEAD
getHomeR = defaultLayout $ do
	$(widgetFile "helloworld")
=======
getHomeR = defaultLayout [whamlet|Hello World! + #{fib 20}|]

fib :: Int -> Int
fib x = (x+30)
>>>>>>> e5d06cb3f4f8098450b15b64dbeafb921fe0deed

main :: IO ()
main = warp 3000 HelloWorld

