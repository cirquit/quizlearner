module TemporaryLibrary where

import Import
import qualified Data.Text as T

data Exam = Exam
    {   exam_title      :: Text
      , exam_max_score  :: Integer --in points
      , exam_max_time   :: Integer --in minutes
      , passing_score   :: Double  --in %
      , exam_questions  :: [Question]
    }
 
data Question = Question
    {  question_id      :: Text
     , question_content :: Text
     , answer_list      :: [Answer]
     , max_score        :: Integer
    }

data Answer = Answer
    {  answer_id       :: Text
     , answer_content  :: Text
     , is_correct      :: Bool
     , is_checked      :: Bool
     , answer_hint     :: Text
    }

-- ###################################################################################
-- Helper

cookie_to_textbool :: Char -> T.Text
cookie_to_textbool x = if x == '1' then T.pack "True"
                                              else T.pack "False"

bool_to_cookie :: [Bool] -> T.Text
bool_to_cookie l = T.pack $ foldr (\x xs -> (bool_to_char x) : xs) "" l
  where bool_to_char :: Bool -> Char
        bool_to_char x = if x then '1' else '0'

show_right_answers :: Question -> T.Text
show_right_answers quest = T.pack $ concatMap ((++ " ") . show . is_correct) (answer_list quest)

-- ###################################################################################
-- Widgets

checkBoxWidget :: T.Text -> T.Text -> T.Text -> Widget
checkBoxWidget checked identity quest = if (T.unpack checked) == "True"
                                         then toWidget [hamlet| <input type=checkbox name=#{T.unpack identity} checked>   </td> <td class=answers> <span class=answers> #{quest} </td>|]
                                         else toWidget [hamlet| <input type=checkbox name=#{T.unpack identity} unchecked> </td> <td class=answers> <span class=answers> #{quest} </td>|]

titleWidget :: Widget
titleWidget = toWidget [hamlet|<a id=title href=@{LayoutR} style="text-decoration:none;">
                                <span style="color:#FAA500;">Quiz</span>Learner|]

-- <img src="images/QuizCreator.png" style="float:right;">

leftWidget :: Widget
leftWidget = toWidget [hamlet|<p id=exam_title> [Exams] </p>
                              <ul id=exam_list>
                                    $forall single_exam <- exams
                                        <li><a href=@{ExamOneR}> #{exam_title single_exam} </a>
 |]

-- ###################################################################################
-- Temporary Exams

exams :: [Exam]
exams =[exam_1, exam_2]

exam_1 :: Exam
exam_1 = Exam {exam_title="Lineare Algebra", exam_max_score=50, exam_max_time=120, passing_score=45.0, exam_questions=[q1,q2,q3,q4,q5,q6,q7,q8,q9]}

exam_2 :: Exam
exam_2 = Exam {exam_title="FFP", exam_max_score=40, exam_max_time=180, passing_score=30.0, exam_questions=[q1]}


-- Temporary Questions
q1,q2,q3,q4,q5,q6,q7,q8,q9 :: Question
q1 = Question {question_id="1", question_content="Wieviel ist 2+3?", answer_list=q1_answers, max_score=4}
q2 = Question {question_id="2", question_content="Wieviel ist 3+4?", answer_list=q2_answers, max_score=4}
q3 = Question {question_id="3", question_content="Wieviel ist 4+5?", answer_list=q3_answers, max_score=4}
q4 = Question {question_id="4", question_content="Wieviel ist 2+3?", answer_list=q4_answers, max_score=4}
q5 = Question {question_id="5", question_content="Wieviel ist 3+4?", answer_list=q5_answers, max_score=4}
q6 = Question {question_id="6", question_content="Wieviel ist 4+5?", answer_list=q6_answers, max_score=4}
q7 = Question {question_id="7", question_content="Wieviel ist 2+3?", answer_list=q7_answers, max_score=4}
q8 = Question {question_id="8", question_content="Wieviel ist 3+4?", answer_list=q8_answers, max_score=4}
q9 = Question {question_id="9", question_content="Wieviel ist 4+5?", answer_list=q9_answers, max_score=4}

-- Temporary Answers
q1_answers, q2_answers, q3_answers, q4_answers, q5_answers, q6_answers, q7_answers, q8_answers, q9_answers :: [Answer]
q1_answers = [q1_a1,q1_a2,q1_a3,q1_a4]
q2_answers = [q2_a1,q2_a2,q2_a3,q2_a4]
q3_answers = [q3_a1,q3_a2,q3_a3,q3_a4]
q4_answers = [q4_a1,q4_a2,q4_a3,q4_a4]
q5_answers = [q5_a1,q5_a2,q5_a3,q5_a4]
q6_answers = [q6_a1,q6_a2,q6_a3,q6_a4]
q7_answers = [q7_a1,q7_a2,q7_a3,q7_a4]
q8_answers = [q8_a1,q8_a2,q8_a3,q8_a4]
q9_answers = [q9_a1,q9_a2,q9_a3,q9_a4]

q1_a1, q1_a2, q1_a3, q1_a4, q2_a1, q2_a2, q2_a3, q2_a4, q3_a1, q3_a2, q3_a3, q3_a4 :: Answer
q4_a1, q4_a2, q4_a3, q4_a4, q5_a1, q5_a2, q5_a3, q5_a4, q6_a1, q6_a2, q6_a3, q6_a4 :: Answer
q7_a1, q7_a2, q7_a3, q7_a4, q8_a1, q8_a2, q8_a3, q8_a4, q9_a1, q9_a2, q9_a3, q9_a4 :: Answer

q1_a1 = Answer {answer_id = "q1_a1", answer_content="Dies ist ein sehr sehr lange Antwort! Dies ist ein sehr sehr lange Antwort! Dies ist ein sehr sehr lange Antwort! Dies ist ein sehr sehr lange Antwort! Dies ist ein sehr sehr lange Antwort! Dies ist ein sehr sehr lange Antwort! Dies ist ein sehr sehr lange Antwort! Dies ist ein sehr sehr lange Antwort!Dies ist ein sehr sehr lange Antwort!", is_correct=False, answer_hint="This is hint", is_checked=False}
q1_a2 = Answer {answer_id = "q1_a2", answer_content="2", is_correct=False, answer_hint="This is hint", is_checked=False}
q1_a3 = Answer {answer_id = "q1_a3", answer_content="4", is_correct=False, answer_hint="This is hint", is_checked=False}
q1_a4 = Answer {answer_id = "q1_a4", answer_content="5", is_correct=True, answer_hint="This is hint", is_checked=False}
q2_a1 = Answer {answer_id = "q2_a1", answer_content="5", is_correct=False, answer_hint="This is hint", is_checked=False}
q2_a2 = Answer {answer_id = "q2_a2", answer_content="2dnauofgbwiegf2974g9f  7 rvze7f9h27zf9qhfvz4r8hfuwhdfuwhfwhef9wh97fgw9rfgbw9gfb9wrgfzuwrbgfwrgbf9wg9wuhfg9uwhrf9wugwh9rugw9urghw9rhgfw98rhg9wrhg9whrg97ewhr97ewhrg9", is_correct=False, answer_hint="This is hint", is_checked=False}
q2_a3 = Answer {answer_id = "q2_a3", answer_content="3", is_correct=False, answer_hint="This is hint", is_checked=False}
q2_a4 = Answer {answer_id = "q2_a4", answer_content="1", is_correct=True, answer_hint="This is hint", is_checked=False}
q3_a1 = Answer {answer_id = "q3_a1", answer_content="1", is_correct=False, answer_hint="This is hint", is_checked=False}
q3_a2 = Answer {answer_id = "q3_a2", answer_content="9", is_correct=False, answer_hint="This is hint", is_checked=False}
q3_a3 = Answer {answer_id = "q3_a3", answer_content="7", is_correct=False, answer_hint="This is hint", is_checked=False}
q3_a4 = Answer {answer_id = "q3_a4", answer_content="2 ", is_correct=True, answer_hint="This is hint", is_checked=False}
q4_a1 = Answer {answer_id = "q4_a1", answer_content="1", is_correct=False, answer_hint="This is hint", is_checked=False}
q4_a2 = Answer {answer_id = "q4_a2", answer_content="2", is_correct=False, answer_hint="This is hint", is_checked=False}
q4_a3 = Answer {answer_id = "q4_a3", answer_content="4", is_correct=False, answer_hint="This is hint", is_checked=False}
q4_a4 = Answer {answer_id = "q4_a4", answer_content="5", is_correct=True, answer_hint="This is hint", is_checked=False}
q5_a1 = Answer {answer_id = "q5_a1", answer_content="5", is_correct=False, answer_hint="This is hint", is_checked=False}
q5_a2 = Answer {answer_id = "q5_a2", answer_content="2", is_correct=False, answer_hint="This is hint", is_checked=False}
q5_a3 = Answer {answer_id = "q5_a3", answer_content="3", is_correct=False, answer_hint="This is hint", is_checked=False}
q5_a4 = Answer {answer_id = "q5_a4", answer_content="1", is_correct=True, answer_hint="This is hint", is_checked=False}
q6_a1 = Answer {answer_id = "q6_a1", answer_content="1", is_correct=False, answer_hint="This is hint", is_checked=False}
q6_a2 = Answer {answer_id = "q6_a2", answer_content="9", is_correct=False, answer_hint="This is hint", is_checked=False}
q6_a3 = Answer {answer_id = "q6_a3", answer_content="7", is_correct=False, answer_hint="This is hint", is_checked=False}
q6_a4 = Answer {answer_id = "q6_a4", answer_content="2", is_correct=True, answer_hint="This is hint", is_checked=False}
q7_a1 = Answer {answer_id = "q7_a1", answer_content="1", is_correct=False, answer_hint="This is hint", is_checked=False}
q7_a2 = Answer {answer_id = "q7_a2", answer_content="2", is_correct=False, answer_hint="This is hint", is_checked=False}
q7_a3 = Answer {answer_id = "q7_a3", answer_content="4", is_correct=False, answer_hint="This is hint", is_checked=False}
q7_a4 = Answer {answer_id = "q7_a4", answer_content="5", is_correct=True, answer_hint="This is hint", is_checked=False}
q8_a1 = Answer {answer_id = "q8_a1", answer_content="5", is_correct=False, answer_hint="This is hint", is_checked=False}
q8_a2 = Answer {answer_id = "q8_a2", answer_content="2", is_correct=False, answer_hint="This is hint", is_checked=False}
q8_a3 = Answer {answer_id = "q8_a3", answer_content="3", is_correct=False, answer_hint="This is hint", is_checked=False}
q8_a4 = Answer {answer_id = "q8_a4", answer_content="1", is_correct=True, answer_hint="This is hint", is_checked=False}
q9_a1 = Answer {answer_id = "q9_a1", answer_content="1", is_correct=False, answer_hint="This is hint", is_checked=False}
q9_a2 = Answer {answer_id = "q9_a2", answer_content="9", is_correct=False, answer_hint="This is hint", is_checked=False}
q9_a3 = Answer {answer_id = "q9_a3", answer_content="7", is_correct=False, answer_hint="This is hint", is_checked=False}
q9_a4 = Answer {answer_id = "q9_a4", answer_content="2", is_correct=True, answer_hint="This is hint", is_checked=False}


