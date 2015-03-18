module Handler.AccManager where

import Import
import Widgets (titleWidget, iconWidget, publicExamWidget, privateExamWidget)
import Assets (getAllExams)

getAccManagerR :: Handler Html
getAccManagerR = do
    memail <- lookupSession "_ID"
    (publicExams, privateExams) <- runDB $ getAllExams memail
    let middleWidget = [whamlet|
        <div style="margin:20px">
            <span class=boldWhite> _{MsgHaveToBeLoggedIn}
                       |]
    defaultLayout $(widgetFile "accmanager")

