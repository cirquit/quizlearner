module Handler.Home where

import Assets (exampleDB)
import Import
import Widgets (titleWidget, iconWidget, leftWidget)

-- | Only for testing purposes
--   If someone deletes all exams, three sample exams are loaded
getHomeR :: Handler Html
getHomeR = do
    setUltDestCurrent
    entityExamList <- runDB (selectList [] [Asc ExamTitle])
    case null entityExamList of
        True  -> runDB $ exampleDB
        False -> liftIO $ putStrLn "DB was not updated, because DB is not empty"
    let middleWidget = [whamlet| <p class=boldWhite>_{MsgClickOnExam} |]
    defaultLayout $(widgetFile "home")