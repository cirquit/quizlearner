module Handler.LineareAlgebra where

import Import
import Data.List as L ((!!), length)
import Data.Text
import System.Random


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

shuffle_answers :: [Answer] -> IO ([Answer])
shuffle_answers list = do
    let n = L.length list
    seed <- getStdRandom (randomR (0, (product [1..n]) - 1))
    return $ permutations list !! seed

exams :: [Exam]
exams =[exam_1, exam_2]

exam_1 :: Exam
exam_1 = Exam {exam_title="Lineare Algebra", exam_max_score=50, exam_max_time=120, passing_score=45.0, exam_questions=[snd_q]}


exam_2:: Exam
exam_2 = Exam {exam_title="FFP", exam_max_score=40, exam_max_time=180, passing_score=30.0, exam_questions=[snd_q]}

snd_q :: Question
snd_q = Question {question_id=1, question_content="Wieviel ist 20/80?", answer_list=snd_qas, max_score=4}

snd_qas :: [Answer]
snd_qas = [snd_a1, snd_a2, snd_a3, snd_a4]

snd_a1 :: Answer
snd_a1 = Answer {answer_id = 00000005, answer_content="0.1", is_correct=False, answer_hint="This is hint", is_checked=False}

snd_a2 :: Answer
snd_a2 = Answer {answer_id = 00000006, answer_content="0.2", is_correct=False, answer_hint="This is hint", is_checked=False}

snd_a3 :: Answer
snd_a3 = Answer {answer_id = 00000007, answer_content="0.4", is_correct=False, answer_hint="This is hint", is_checked=False}

snd_a4 :: Answer
snd_a4 = Answer {answer_id = 00000008, answer_content="0.5", is_correct=True, answer_hint="This is hint", is_checked=False}


getLineareAlgebraR :: Handler Html
getLineareAlgebraR = defaultLayout $ do
  setTitle "Lin Alg"
  shuffled_answers <- liftIO $ shuffle_answers $ answer_list snd_q
  $(widgetFile "linearealgebra")


postLineareAlgebraR :: Handler Html
postLineareAlgebraR = error "Not yet implemented: postLineareAlgebraR"