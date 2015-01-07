module Handler.Layout where

import Import
import TemporaryLibrary
import Database.Persist.Sqlite -- redundant but somehow important


middleWidget :: Widget
middleWidget = toWidget [hamlet| <p id=startScreen>Click on an exam to start! |]

getLayoutR :: Handler Html
getLayoutR = defaultLayout $ do
  setTitle "Basic Layout"
  _ <- liftIO $ load_DB
  entity_exam_list <- runSqlite "develTest.sqlite3" (selectList [] [Asc ExamExam_max_time])
  $(widgetFile "layout")