module Handler.Exam where

import TemporaryLibrary
import Import

getExamR :: Text -> Handler Html
getExamR exam_name = defaultLayout $ do $(widgetFile "exam")


middleWidget :: Text -> Widget
middleWidget exam_name = do
      toWidget [hamlet| <a href=@{ExamR exam_name} id="testing_exams"> #{exam_name} |]
      toWidget [lucius| #testing_exams {
                           font-size: 50px;
                           font-family: Helvetica, Arial;
                           color: #FFA500;
                        }
               |]