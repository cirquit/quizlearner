--import Prelude     (IO)
--import Application (appMain)

--main :: IO ()
--main = appMain
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


module Main where

import Yesod

data App = App
instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "File Processor"
    toWidget [whamlet|
<h2>Previously submitted files
|]

main :: IO ()
main = warpEnv App