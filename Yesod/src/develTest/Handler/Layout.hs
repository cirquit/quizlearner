module Handler.Layout where

import Data.List ((!!), length)
import Data.Text hiding (length)
import Import hiding (length)
import System.Random


data Test = Test
    {   title :: Text
      , score :: Integer        --in points
      , time  :: Integer        --in minutes
      , passing_score :: Double --in %
      , questions :: [Question] --
    }

data Question = Question
    {  number  :: Integer
     , question:: Text
     , answers :: [Answer]
    }

data Answer = Answer
    {  answer  :: Text
     , correct :: Bool
     , hint    :: Text
    }

shuffle_as :: [Answer] -> IO ([Answer])
shuffle_as list = do
                     let n = length list
                     seed <- getStdRandom (randomR (0, product [1..n]))
                     return $ permutations list !! seed



fst_q :: Question
fst_q = Question {number=1, question="Wieviel ist 2+3?", answers=fst_qas}

fst_qas = [fst_a1, fst_a2, fst_a3, fst_a4]

fst_a1 :: Answer
fst_a1 = Answer {answer="1", correct=False, hint="This is hint"}

fst_a2 :: Answer
fst_a2 = Answer {answer="2", correct=False, hint="This is hint"}

fst_a3 :: Answer
fst_a3 = Answer {answer="4", correct=False, hint="This is hint"}

fst_a4 :: Answer
fst_a4 = Answer {answer="5", correct=True, hint="This is hint"}



getLayoutR :: Handler Html
getLayoutR = defaultLayout $ do
  setTitle "Basic Layout"
  test <- liftIO $ shuffle_as $ answers fst_q
  $(widgetFile "layout")
