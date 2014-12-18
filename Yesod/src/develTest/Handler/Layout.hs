module Handler.Layout where

import Data.List as L ((!!), length)
import Data.Text
import Import
import System.Random


data Test = Test
    {   test_title      :: Text
      , test_max_score  :: Integer --in points
      , test_max_time   :: Integer --in minutes
      , passing_score   :: Double  --in %
      , test_questions  :: [Question]
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



fst_q :: Question
fst_q = Question {question_id=1, question_content="Wieviel ist 2+3?", answer_list=fst_qas}

fst_qas = [fst_a1, fst_a2, fst_a3, fst_a4]

fst_a1 :: Answer
fst_a1 = Answer {answer_id = 00000001, answer_content="1", is_correct=False, answer_hint="This is hint", is_checked=False}

fst_a2 :: Answer
fst_a2 = Answer {answer_id = 00000002, answer_content="2", is_correct=False, answer_hint="This is hint", is_checked=False}

fst_a3 :: Answer
fst_a3 = Answer {answer_id = 00000003, answer_content="4", is_correct=False, answer_hint="This is hint", is_checked=False}

fst_a4 :: Answer
fst_a4 = Answer {answer_id = 00000004, answer_content="5", is_correct=True, answer_hint="This is hint", is_checked=False}



getLayoutR :: Handler Html
getLayoutR = defaultLayout $ do
  setTitle "Basic Layout"
  shuffled_answers <- liftIO $ shuffle_answers $ answer_list fst_q
  $(widgetFile "layout")

