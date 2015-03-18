module Handler.Home where

import Assets (exampleDB, getAllExams)
import Import
import Widgets (titleWidget, iconWidget, publicExamWidget, privateExamWidget)

-- | Only for testing purposes
--   If someone deletes all exams, three sample exams are loaded
getHomeR :: Handler Html
getHomeR = do
    setUltDestCurrent
    memail <- lookupSession "_ID"
    (publicExams, privateExams) <- runDB $ getAllExams memail
    case null publicExams of
        True  -> runDB $ exampleDB
        False -> liftIO $ putStrLn "DB was not updated, because DB is not empty"
    let middleWidget = [whamlet| <p class=boldWhite>_{MsgClickOnExam} |]
    defaultLayout $(widgetFile "home")