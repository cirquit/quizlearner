module Handler.Home where

import Import
import Assets (loadDB, titleWidget, iconWidget, leftWidget)

middleWidget :: Widget
middleWidget = toWidget [hamlet| <p class=boldWhite> Click on an exam to start! |]

getHomeR :: Handler Html
getHomeR = do
        --runDB (deleteWhere [ExamTitle !=. ""])
        entityExamList <- runDB (selectList [] [Desc ExamTitle])
        if null entityExamList then runDB $ loadDB
                               else liftIO $ putStrLn "loadDB was not called because there are already some exams!"
        defaultLayout $ do
            setTitle "Quizlearner"
            $(widgetFile "home")