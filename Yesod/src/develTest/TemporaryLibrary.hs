module TemporaryLibrary where

import Import
import Data.List ((!!))


data Test_Answer = Test_Answer
    {   checkbox_1 :: Bool
      , checkbox_2 :: Bool
      , checkbox_3 :: Bool
      , checkbox_4 :: Bool
    }

instance Show Test_Answer where
  show (Test_Answer a b c d) = "Box 1 = " ++ (show a) ++ " | " ++
                               "Box 2 = " ++ (show b) ++ " | " ++
                               "Box 3 = " ++ (show c) ++ " | " ++
                               "Box 4 = " ++ (show d) ++ " | "
data Exam = Exam
    {   exam_title      :: Text
      , exam_max_score  :: Integer --in points
      , exam_max_time   :: Integer --in minutes
      , passing_score   :: Double  --in %
      , exam_questions  :: [Question]
    }

data Question = Question
    {  question_id      :: Integer
     , question_content :: Text
     , answer_list      :: [Answer]
     , max_score        :: Integer
    }

data Answer = Answer
    {  answer_id       :: Integer
     , answer_content  :: Text
     , is_correct      :: Bool
     , is_checked      :: Bool
     , answer_hint     :: Text
    }

data Returned_Answers = Returned_Answers {res1_1, res1_2, res1_3, res1_4,
                                          res2_1, res2_2, res2_3, res2_4,
                                          res3_1, res3_2, res3_3, res3_4 :: Bool}

hip :: Bool -> String
hip x = if x then "O" else "X"

instance Show Returned_Answers where
  show (Returned_Answers a b c d e f g h i j k l) = " | Q1 >> a1=" ++ hip a ++ " | a2= " ++ hip b ++ " | a3= " ++ hip c ++ " | a4=" ++ hip d ++
                                                    " | Q2 >> a1=" ++ hip e ++ " | a2= " ++ hip f ++ " | a3= " ++ hip g ++ " | a4=" ++ hip h ++
                                                    " | Q3 >> a1=" ++ hip i ++ " | a2= " ++ hip j ++ " | a3= " ++ hip k ++ " | a4=" ++ hip l



-- Temporary Exams
exams :: [Exam]
exams =[exam_1, exam_2]

exam_1 :: Exam
exam_1 = Exam {exam_title="Lineare Algebra", exam_max_score=50, exam_max_time=120, passing_score=45.0, exam_questions=[fst_q, snd_q, thd_q]}

exam_2 :: Exam
exam_2 = Exam {exam_title="FFP", exam_max_score=40, exam_max_time=180, passing_score=30.0, exam_questions=[fst_q]}


-- Temporary Questions
fst_q :: Question
fst_q = Question {question_id=1, question_content="Wieviel ist 2+3?", answer_list=fst_qas, max_score=4}

snd_q :: Question
snd_q = Question {question_id=2, question_content="Wieviel ist 3+4?", answer_list=snd_qas, max_score=4}

thd_q :: Question
thd_q = Question {question_id=3, question_content="Wieviel ist 4+5?", answer_list=thd_qas, max_score=4}


-- Temporary Answers
fst_qas :: [Answer]
fst_qas = [fst_a1, fst_a2, fst_a3, fst_a4]

snd_qas :: [Answer]
snd_qas = [snd_a1, snd_a2, snd_a3, snd_a4]

thd_qas :: [Answer]
thd_qas = [trd_a1, trd_a2, trd_a3, trd_a4]

fst_a1 :: Answer
fst_a1 = Answer {answer_id = 1, answer_content="1", is_correct=False, answer_hint="This is hint", is_checked=False}
fst_a2 :: Answer
fst_a2 = Answer {answer_id = 2, answer_content="2", is_correct=False, answer_hint="This is hint", is_checked=False}
fst_a3 :: Answer
fst_a3 = Answer {answer_id = 3, answer_content="4", is_correct=False, answer_hint="This is hint", is_checked=False}
fst_a4 :: Answer
fst_a4 = Answer {answer_id = 4, answer_content="5", is_correct=True, answer_hint="This is hint", is_checked=False}

snd_a1 :: Answer
snd_a1 = Answer {answer_id = 5, answer_content="1", is_correct=False, answer_hint="This is hint", is_checked=False}
snd_a2 :: Answer
snd_a2 = Answer {answer_id = 6, answer_content="2", is_correct=False, answer_hint="This is hint", is_checked=False}
snd_a3 :: Answer
snd_a3 = Answer {answer_id = 7, answer_content="4", is_correct=False, answer_hint="This is hint", is_checked=False}
snd_a4 :: Answer
snd_a4 = Answer {answer_id = 8, answer_content="5", is_correct=True, answer_hint="This is hint", is_checked=False}

trd_a1 :: Answer
trd_a1 = Answer {answer_id = 9, answer_content="1", is_correct=False, answer_hint="This is hint", is_checked=False}
trd_a2 :: Answer
trd_a2 = Answer {answer_id = 10, answer_content="2", is_correct=False, answer_hint="This is hint", is_checked=False}
trd_a3 :: Answer
trd_a3 = Answer {answer_id = 11, answer_content="4", is_correct=False, answer_hint="This is hint", is_checked=False}
trd_a4 :: Answer
trd_a4 = Answer {answer_id = 12, answer_content="5", is_correct=True, answer_hint="This is hint", is_checked=False}

