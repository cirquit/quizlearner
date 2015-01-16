module Handler.Layout where

import Import
import TemporaryLibrary


middleWidget :: Widget
middleWidget = toWidget [hamlet| <p class=boldWhite>Click on an exam to start! |]

getLayoutR :: Handler Html
getLayoutR = do
        entity_exam_list <- runDB (selectList [] [Desc ExamTitle])

        if null entity_exam_list then runDB $ load_DB
                                 else liftIO $ putStrLn $ col_cyan ++ "load_DB was not called because there are already some exams!"

        defaultLayout $ do
         setTitle "Basic Layout"
--       runSqlite "develTest.sqlite3" (deleteWhere [ExamTitle !=. ""])
--       liftIO $ load_DB
         $(widgetFile "layout")