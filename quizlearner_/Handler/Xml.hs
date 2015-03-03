module Handler.Xml where

import Assets
import Import

getXmlR :: ExamId -> Handler Html
getXmlR exam = do
    entityExamList <- runDB $ selectList [] [Desc ExamTitle]
    let middleWidget = [whamlet|
                         <p class=boldWhite> This is xml - test
                       |]
    defaultLayout $ do $(widgetFile "xml")