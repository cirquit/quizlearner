module TemporaryLibrary where

import Import

data Test_Answer = Test_Answer
    {   checkbox_1 :: Bool
      , checkbox_2 :: Bool
      , checkbox_3 :: Bool
      , checkbox_4 :: Bool
    }

instance Show Test_Answer where
  show (Test_Answer a b c d) = "Box 1 = " ++ (show a) ++ "\n" ++
                               "Box 2 = " ++ (show b) ++ "\n" ++
                               "Box 3 = " ++ (show c) ++ "\n" ++
                               "Box 4 = " ++ (show d) ++ "\n"
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
snd_qas = [fst_a1, fst_a2, fst_a3, fst_a4]

thd_qas :: [Answer]
thd_qas = [fst_a1, fst_a2, fst_a3, fst_a4]

fst_a1 :: Answer
fst_a1 = Answer {answer_id = 00000001, answer_content="1", is_correct=False, answer_hint="This is hint", is_checked=False}

fst_a2 :: Answer
fst_a2 = Answer {answer_id = 00000002, answer_content="2", is_correct=False, answer_hint="This is hint", is_checked=False}

fst_a3 :: Answer
fst_a3 = Answer {answer_id = 00000003, answer_content="4", is_correct=False, answer_hint="This is hint", is_checked=False}

fst_a4 :: Answer
fst_a4 = Answer {answer_id = 00000004, answer_content="5", is_correct=True, answer_hint="This is hint", is_checked=False}
