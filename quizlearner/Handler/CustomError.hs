module Handler.CustomError where

import Assets (getAllExams)
import Import
import Widgets (titleWidget, iconWidget, publicExamWidget, privateExamWidget)

getCustomErrorR :: Handler Html
getCustomErrorR = do
    setUltDestCurrent
    memail <- lookupSession "_ID"
    (publicExams, privateExams) <- runDB $ getAllExams memail
    let middleWidget = [whamlet| <p class=boldWhite>_{MsgGet404} |]
    defaultLayout $(widgetFile "error")