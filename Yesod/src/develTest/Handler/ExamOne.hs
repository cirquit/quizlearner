module Handler.ExamOne where

import Import
import TemporaryLibrary
import Data.List ((!!))
import qualified Data.Text as T

-- ###############################################################################
-- ONLY FOR GET

middleWidget_GET :: Widget
middleWidget_GET = [whamlet|<form method=post>
                              <ul class="tabs">
                                            ^{create_question fst_q} <!-- this must be done for every question -->
                                            ^{create_question snd_q}
                                            ^{create_question trd_q}
                               <input type=submit id=submit_button value="Save!">

                    |]

create_question :: Question -> Widget
create_question quest = do
        m_answers <- lookupSession (question_id quest)
        case (m_answers) of
            (Just (T.unpack -> [x1,x2,x3,x4])) -> [whamlet|
         <li>
             <input type="radio" name="tabs" id="tab#{question_id quest}">
             <label for="tab#{question_id quest}">Question #{question_id quest}</label>
             <div id="tab-content1" class="tab-content animated fadeIn">
               <table>
                  <tr>
                    <th> Question #{question_id quest}
                  <tr>
                    <th> #{question_content quest}
                  <tr>
                    <td> ^{checkBoxWidget (cookie_to_textbool x1) (answer_id ((answer_list quest) !! 0)) (answer_content ((answer_list quest) !! 0))}
                  <tr>
                    <td> ^{checkBoxWidget (cookie_to_textbool x2) (answer_id ((answer_list quest) !! 1)) (answer_content ((answer_list quest) !! 1))}
                  <tr>
                    <td> ^{checkBoxWidget (cookie_to_textbool x3) (answer_id ((answer_list quest) !! 2)) (answer_content ((answer_list quest) !! 2))}
                  <tr>
                    <td> ^{checkBoxWidget (cookie_to_textbool x4) (answer_id ((answer_list quest) !! 3)) (answer_content ((answer_list quest) !! 3))}
                                                  |]

            _                            -> [whamlet|
           <li>
             <input type="radio" name="tabs" id="tab#{question_id quest}">
             <label for="tab#{question_id quest}">Question #{question_id quest}</label>
             <div id="tab-content1" class="tab-content animated fadeIn">
               <table>
                  <tr>
                    <th> Question #{question_id quest}
                  <tr>
                    <th> #{question_content quest}
                  <tr>
                    <td> ^{checkBoxWidget (T.pack "False") (answer_id ((answer_list quest) !! 0)) (answer_content ((answer_list quest) !! 0))}
                  <tr>
                    <td> ^{checkBoxWidget (T.pack "False") (answer_id ((answer_list quest) !! 1)) (answer_content ((answer_list quest) !! 1))}
                  <tr>
                    <td> ^{checkBoxWidget (T.pack "False") (answer_id ((answer_list quest) !! 2)) (answer_content ((answer_list quest) !! 2))}
                  <tr>
                    <td> ^{checkBoxWidget (T.pack "False") (answer_id ((answer_list quest) !! 3)) (answer_content ((answer_list quest) !! 3))}
                                                   |]

-- ###############################################################################
-- ONLY FOR POST

save_cur_answers :: Question -> WidgetT App IO()
save_cur_answers quest = do
            q_box1 <- runInputPost $ ireq checkBoxField (answer_id ((answer_list quest) !! 0))
            q_box2 <- runInputPost $ ireq checkBoxField (answer_id ((answer_list quest) !! 1))
            q_box3 <- runInputPost $ ireq checkBoxField (answer_id ((answer_list quest) !! 2))
            q_box4 <- runInputPost $ ireq checkBoxField (answer_id ((answer_list quest) !! 3))

            setSession (question_id quest) $ bool_to_cookie [q_box1, q_box2, q_box3, q_box4]


validate_answers :: Question -> Widget
validate_answers quest = do
            m_answers <- lookupSession (question_id quest)
            case (m_answers) of
                (Just (T.unpack -> [x1,x2,x3,x4])) -> toWidget [hamlet|
                          <span id=evaluation> Question#{question_id quest} #{question_content quest} should be:
                          <span id=evaluation> #{show_right_answers quest} <br>
                          <span id=evaluation> You said that A1 := #{answer_content ((answer_list quest) !! 0)} == #{cookie_to_textbool x1} <br>
                          <span id=evaluation> You said that A2 := #{answer_content ((answer_list quest) !! 1)} == #{cookie_to_textbool x2} <br>
                          <span id=evaluation> You said that A3 := #{answer_content ((answer_list quest) !! 2)} == #{cookie_to_textbool x3} <br>
                          <span id=evaluation> You said that A4 := #{answer_content ((answer_list quest) !! 3)} == #{cookie_to_textbool x4} <br>
                          |]
                _                                  -> toWidget[hamlet| <span id=evaluation> There are no current answers submitted|]

middleWidget_POST :: Widget
middleWidget_POST = do
            save_cur_answers fst_q -- this must be done for every question
            save_cur_answers snd_q
            save_cur_answers trd_q
            [whamlet| <span id=evaluation> I hopefully saved all of your answers! <br>
                        ^{validate_answers fst_q}   <!-- this must be done for every question -->
                        ^{validate_answers snd_q}
                      <span id=evaluation> <a href=@{ExamOneR}> Get back! </a>
                     |]

-- ###############################################################################
-- TO ADD NEW QUESTION EDIT
-- ROW 14 | ROW 94 | ROW 98

getExamOneR :: Handler Html
getExamOneR = defaultLayout $ do $(widgetFile "exam_form")

postExamOneR :: Handler Html
postExamOneR = defaultLayout $ do $(widgetFile "exam_answers")