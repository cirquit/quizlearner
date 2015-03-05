module Handler.Home where

import Import
import Assets (exampleDB)
import Widgets (titleWidget, iconWidget, leftWidget)

getHomeR :: Handler Html
getHomeR = do
    --runDB (deleteWhere [ExamTitle !=. ""])
    entityExamList <- runDB (selectList [] [Asc ExamTitle])
    if null entityExamList then runDB $ exampleDB
                           else liftIO $ putStrLn "loadDB was not called because there are already some exams!"
    let middleWidget = [whamlet| <p class=boldWhite> Click on an exam to start!|]
    defaultLayout $ do
        setTitle "Quizlearner"
        $(widgetFile "home")