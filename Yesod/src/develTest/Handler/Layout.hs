module Handler.Layout where

import Import
import TemporaryLibrary

getLayoutR :: Handler Html
getLayoutR = defaultLayout $ do
  setTitle "Basic Layout"
  $(widgetFile "layout")