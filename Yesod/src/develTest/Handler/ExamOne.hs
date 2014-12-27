module Handler.ExamOne where

import Import
import TemporaryLibrary

getExamOneR :: Handler Html
getExamOneR = do
    (widget, enctype) <- generateFormPost answer_form
    defaultLayout $
     $(widgetFile "exam_form")


postExamOneR :: Handler Html
postExamOneR = do
   ((result, widget), enctype) <- runFormPost answer_form
   case result of
       FormSuccess test_answers -> defaultLayout $
             $(widgetFile "exam_answers")
       _ -> defaultLayout $ [whamlet|
                             <p> Error! You dun goofed...this should never happen.
                             <a id=exam_title href=@{ExamOneR}> Get back!>
                            |]

test_answer_AForm :: AForm Handler Test_Answer
test_answer_AForm = Test_Answer
  <$> areq checkBoxField "Testing...1" Nothing
  <*> areq checkBoxField "Testing...2" Nothing
  <*> areq checkBoxField "Testing...3" Nothing
  <*> areq checkBoxField "Testing...4" Nothing

answer_form :: Form Test_Answer
answer_form = renderTable test_answer_AForm