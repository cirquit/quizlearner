module Handler.Layout where

import Import
import TemporaryLibrary
import Database.Persist.Sqlite -- redundant but somehow needed


middleWidget :: Widget
middleWidget = toWidget [hamlet| <p id=startScreen>Click on an exam to start! |]

getLayoutR :: Handler Html
getLayoutR = defaultLayout $ do
  setTitle "Basic Layout"

  entity_exam_list <- runSqlite "develTest.sqlite3" (selectList [] [Asc ExamExam_title])

  --if null entity_exam_list then liftIO $ load_DB
  --                         else liftIO $ putStrLn $ col_cyan ++ "load_DB was not called because there are already some exams!" ++ col_reset
  runSqlite "develTest.sqlite3" (deleteWhere [ExamExam_title !=. ""])
  liftIO $ load_DB


  $(widgetFile "layout")