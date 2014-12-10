module Handler.Echo where


import Prelude (read)
import Import

getEchoR :: Text -> Handler Html
getEchoR theText = 
  defaultLayout $(widgetFile "echo")