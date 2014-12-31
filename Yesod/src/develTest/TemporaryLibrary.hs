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
                                         then toWidget [hamlet| <input type=checkbox name=#{T.unpack identity} checked>   <span id=answers> #{quest} <br>|]
                                         else toWidget [hamlet| <input type=checkbox name=#{T.unpack identity} unchecked> <span id=answers> #{quest} <br>|]

titleWidget :: Widget
titleWidget = toWidget [hamlet|<a id=title href=@{LayoutR}> QuizLearner |]

leftWidget :: Widget
leftWidget = toWidget [hamlet| <ul id=exam_list>
                                    <li><p id=exam_title> [Exams] </p>
                                    $forall single_exam <- exams
                                        <li><a class=button href=@{ExamOneR}> #{exam_title single_exam} </a>
 |]

-- ###################################################################################
-- Temporary Exams

exams :: [Exam]
exams =[exam_1, exam_2]

exam_1 :: Exam
exam_1 = Exam {exam_title="Lineare Algebra", exam_max_score=50, exam_max_time=120, passing_score=45.0, exam_questions=[fst_q, snd_q, trd_q]}

exam_2 :: Exam
exam_2 = Exam {exam_title="FFP", exam_max_score=40, exam_max_time=180, passing_score=30.0, exam_questions=[fst_q]}


-- Temporary Questions
fst_q :: Question
fst_q = Question {question_id="Nr.1", question_content="Wieviel ist 2+3?", answer_list=fst_qas, max_score=4}

snd_q :: Question
snd_q = Question {question_id="Nr.2", question_content="Wieviel ist 3+4?", answer_list=snd_qas, max_score=4}

trd_q :: Question
trd_q = Question {question_id="Nr.3", question_content="Wieviel ist 4+5?", answer_list=trd_qas, max_score=4}


-- Temporary Answers
fst_qas :: [Answer]
fst_qas = [fst_a1, fst_a2, fst_a3, fst_a4]

snd_qas :: [Answer]
snd_qas = [snd_a1, snd_a2, snd_a3, snd_a4]

trd_qas :: [Answer]
trd_qas = [trd_a1, trd_a2, trd_a3, trd_a4]

fst_a1 :: Answer
fst_a1 = Answer {answer_id = "fst_q_a1", answer_content="1", is_correct=False, answer_hint="This is hint", is_checked=False}
fst_a2 :: Answer
fst_a2 = Answer {answer_id = "fst_q_a2", answer_content="2", is_correct=False, answer_hint="This is hint", is_checked=False}
fst_a3 :: Answer
fst_a3 = Answer {answer_id = "fst_q_a3", answer_content="4", is_correct=False, answer_hint="This is hint", is_checked=False}
fst_a4 :: Answer
fst_a4 = Answer {answer_id = "fst_q_a4", answer_content="5", is_correct=True, answer_hint="This is hint", is_checked=False}

snd_a1 :: Answer
snd_a1 = Answer {answer_id = "snd_q_a1", answer_content="5", is_correct=False, answer_hint="This is hint", is_checked=False}
snd_a2 :: Answer
snd_a2 = Answer {answer_id = "snd_q_a2", answer_content="2", is_correct=False, answer_hint="This is hint", is_checked=False}
snd_a3 :: Answer
snd_a3 = Answer {answer_id = "snd_q_a3", answer_content="3", is_correct=False, answer_hint="This is hint", is_checked=False}
snd_a4 :: Answer
snd_a4 = Answer {answer_id = "snd_q_a4", answer_content="1", is_correct=True, answer_hint="This is hint", is_checked=False}

trd_a1 :: Answer
trd_a1 = Answer {answer_id = "trd_q_a1", answer_content="13", is_correct=False, answer_hint="This is hint", is_checked=False}
trd_a2 :: Answer
trd_a2 = Answer {answer_id = "trd_q_a2", answer_content="9", is_correct=False, answer_hint="This is hint", is_checked=False}
trd_a3 :: Answer
trd_a3 = Answer {answer_id = "trd_q_a3", answer_content="7", is_correct=False, answer_hint="This is hint", is_checked=False}
trd_a4 :: Answer
trd_a4 = Answer {answer_id = "trd_q_a4", answer_content="20", is_correct=True, answer_hint="This is hint", is_checked=False}

