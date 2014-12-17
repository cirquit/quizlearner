{-# LANGUAGE ForeignFunctionInterface #-}

module Handler.Layout where

import Data.List ((!!))
import Data.Text
import Import hiding (length)
import System.Random


data Test = Test
    {  

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

--shuffle_as :: [Answer] -> IO ([Answer])
--shuffle_as list = do
--                     let n = length list
--                     seed <- getStdRandom (randomR (0, product [1..n]))
--                     return $ permutations l !! seed



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
  $(widgetFile "layout")
