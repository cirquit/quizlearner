module Handler.Layout where

import Import

getLayoutR :: Handler Html
getLayoutR = defaultLayout $ do
	setTitle "Basic Layout"
	$(widgetFile "layout")
