module Handler.Home where

import Import
import Assets (exampleDB)
import Widgets (titleWidget, iconWidget, leftWidget)

getHomeR :: Handler Html
getHomeR = do
    --runDB (deleteWhere [ExamTitle !=. ""])
    setUltDestCurrent
    entityExamList <- runDB (selectList [] [Asc ExamTitle])
    if null entityExamList then runDB $ exampleDB
                           else liftIO $ putStrLn "DB was not updated, because DB is not empty"
    let middleWidget = [whamlet| <p class=boldWhite>_{MsgClickOnExam}|]
    defaultLayout $ do
        setTitle "Quizlearner"
        $(widgetFile "home")