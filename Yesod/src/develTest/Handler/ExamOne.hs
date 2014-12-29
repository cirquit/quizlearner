module Handler.ExamOne where

import Import
import TemporaryLibrary
import Data.List ((!!))

getExamOneR :: Handler Html
getExamOneR = do --(widget, enctype) <- generateFormPost $ answer_form
                  defaultLayout $ do $(widgetFile "exam_form")

postExamOneR :: Handler Html
postExamOneR = do
        result <- runInputPost $ Returned_Answers
                                 <$> ireq checkBoxField "1"
                                 <*> ireq checkBoxField "2"
                                 <*> ireq checkBoxField "3"
                                 <*> ireq checkBoxField "4"
                                 <*> ireq checkBoxField "5"
                                 <*> ireq checkBoxField "6"
                                 <*> ireq checkBoxField "7"
                                 <*> ireq checkBoxField "8"
                                 <*> ireq checkBoxField "9"
                                 <*> ireq checkBoxField "10"
                                 <*> ireq checkBoxField "11"
                                 <*> ireq checkBoxField "12"
        defaultLayout $ do $(widgetFile "exam_answers")
--   ((result, widget), enctype) <- runFormPost $ answer_form
--   case result of
--       FormSuccess test_answers -> defaultLayout $
--             $(widgetFile "exam_answers")
--       _ -> defaultLayout $ [whamlet|
--                             <p> Error! You dun goofed...this should never happen.
--                             <a id=exam_title href=@{ExamOneR}> Get back!>
--                            |]

--test_answer_AForm :: AForm Handler Test_Answer
--test_answer_AForm = Test_Answer
--  <$> areq checkBoxField "Nothing..." Nothing
--  <*> areq checkBoxField "Nothing..." Nothing
--  <*> areq checkBoxField "Nothing..." Nothing
--  <*> areq checkBoxField "Nothing..." Nothing


--answer_form :: Form Test_Answer
--answer_form = renderTable $ test_answer_AForm