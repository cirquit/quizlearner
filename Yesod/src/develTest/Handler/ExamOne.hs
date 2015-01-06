module Handler.ExamOne where

import Import
import TemporaryLibrary
import Data.List ((!!))
import qualified Data.Text as T


middleWidget_GET :: Widget
middleWidget_GET = [whamlet|<form method=post>
                                <ul class="tabs">
                                           $forall question <- exam_questions exam_1
                                             ^{create_question question}
                              <input type=submit id=submit_button value="Save!">

                    |]

create_question :: Question -> Widget
create_question quest = do
        m_answers <- lookupSession (question_id quest)
        case (m_answers) of
            (Just (T.unpack -> [x1,x2,x3,x4])) -> [whamlet|
         <li>
             <input type="radio" name="tabs" id="tab#{question_id quest}">
             <label for="tab#{question_id quest}">Q #{question_id quest}
             <div id="tab-content#{question_id quest}" class="tab-content animated fadeIn">
              <span class=question_head> Question #{question_id quest}
              <br>
              <span class=question_head> #{question_content quest}
              <table>
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
             <label for="tab#{question_id quest}">Q #{question_id quest}
             <div id="tab-content#{question_id quest}" class="tab-content animated fadeIn">
              <span class=question_head> Question #{question_id quest}
              <br>
              <span class=question_head> #{question_content quest}
              <table>
                 <tr>
                   <td> ^{checkBoxWidget (T.pack "False") (answer_id ((answer_list quest) !! 0)) (answer_content ((answer_list quest) !! 0))}
                 <tr>
                   <td> ^{checkBoxWidget (T.pack "False") (answer_id ((answer_list quest) !! 1)) (answer_content ((answer_list quest) !! 1))}
                 <tr>
                   <td> ^{checkBoxWidget (T.pack "False") (answer_id ((answer_list quest) !! 2)) (answer_content ((answer_list quest) !! 2))}
                 <tr>
                   <td> ^{checkBoxWidget (T.pack "False") (answer_id ((answer_list quest) !! 3)) (answer_content ((answer_list quest) !! 3))}
                                                   |]


save_cur_answers :: Question -> WidgetT App IO() 
save_cur_answers quest = do
            q_box1 <- runInputPost $ ireq checkBoxField (answer_id ((answer_list quest) !! 0))
            q_box2 <- runInputPost $ ireq checkBoxField (answer_id ((answer_list quest) !! 1))
            q_box3 <- runInputPost $ ireq checkBoxField (answer_id ((answer_list quest) !! 2))
            q_box4 <- runInputPost $ ireq checkBoxField (answer_id ((answer_list quest) !! 3))

            setSession (question_id quest) $ bool_to_cookie [q_box1, q_box2, q_box3, q_box4]

save_all :: WidgetT App IO[()]
save_all = mapM save_cur_answers (exam_questions exam_1)

validate_answers :: Question -> Widget
validate_answers quest = do
            m_answers <- lookupSession (question_id quest)
            case (m_answers) of
                (Just (T.unpack -> [x1,x2,x3,x4])) -> toWidget [hamlet|
                          <span class=evaluation> Question#{question_id quest} #{question_content quest} should be:
                          <span class=evaluation> #{show_right_answers quest} <br>
                          <span class=evaluation> You said that A1 := #{answer_content ((answer_list quest) !! 0)} == #{cookie_to_textbool x1} <br>
                          <span class=evaluation> You said that A2 := #{answer_content ((answer_list quest) !! 1)} == #{cookie_to_textbool x2} <br>
                          <span class=evaluation> You said that A3 := #{answer_content ((answer_list quest) !! 2)} == #{cookie_to_textbool x3} <br>
                          <span class=evaluation> You said that A4 := #{answer_content ((answer_list quest) !! 3)} == #{cookie_to_textbool x4} <br>
                          |]
                _                                  -> toWidget[hamlet| <span class=evaluation> There are no current answers submitted|]

middleWidget_POST :: Widget
middleWidget_POST = do
             _ <- mapM save_cur_answers (exam_questions exam_1)
             [whamlet| <span class=evaluation> I hopefully saved all of your answers! <br>
                         $forall question <- exam_questions exam_1
                             ^{validate_answers question}
                        <span class=evaluation> <a href=@{ExamOneR}> Get back! </a>
                     |]


getExamOneR :: Handler Html
getExamOneR = defaultLayout $ do $(widgetFile "exam_form")

postExamOneR :: Handler Html
postExamOneR = defaultLayout $ do $(widgetFile "exam_answers")