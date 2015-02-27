module Handler.Quizcreator where

import Import
import Assets (loadDB, titleWidget, iconWidget, leftWidget)

getQuizcreatorR :: Handler Html
getQuizcreatorR =  do
                   entityExamList <- runDB $ selectList [] [Desc ExamTitle]
                   defaultLayout $ do $(widgetFile "quizcreator")

postQuizcreatorR :: Handler Html
postQuizcreatorR = do
                   entityExamList <- runDB $ selectList [] [Desc ExamTitle]
                   defaultLayout $ do $(widgetFile "quizcreator")
