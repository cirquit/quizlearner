module Handler.AccManager where

import Widgets
import Import

getAccManagerR :: Handler Html
getAccManagerR = do
    entityExamList <- runDB (selectList [] [Asc ExamTitle])
    let middleWidget = [whamlet| <div style="margin:20px">
                                     <span class=boldWhite> You have to be logged in to delete exams!
                                     |]
    defaultLayout $ do $(widgetFile "accmanager")

