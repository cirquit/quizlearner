module Handler.Layout where

import Import
import TemporaryLibrary

middleWidget :: Widget
middleWidget = toWidget [hamlet| <p id=startScreen>Click on an exam to start! |]

getLayoutR :: Handler Html
getLayoutR = defaultLayout $ do
  setTitle "Basic Layout"
  $(widgetFile "layout")