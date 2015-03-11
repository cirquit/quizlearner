{-# LANGUAGE Arrows, OverloadedStrings #-}

import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG
import Data.Text (Text, pack, unpack)
import Data.Maybe (fromMaybe)
import Data.List.Split (chunksOf)

data Exam = Exam {examTitle :: Text, examPassPercetage :: Double, examQuestions :: [Question]}
    deriving Show

data Question = Question {questionContent :: Text, questionAnswerList :: [Answer]}
    deriving Show

data Answer = Answer {answerContent :: Text, answerIsCorrect :: Bool}
    deriving Show


readPassPercentage :: String -> Double
readPassPercentage s = case reads s of
                        [(x, "")] -> x
                        _         -> -1


makeE "quiz" = Exam
makeE trans = error $ "Invalid transaction type: " ++ trans

makeQ "question" = Question
makeQ trans = error $ "Invalid transaction type: " ++ trans

makeA "answer" = Answer
makeA trans = error $ "Invalid transaction type: " ++ trans


selectA = getChildren >>> getChildren >>> getChildren >>> isElem
selectQ = getChildren >>> getChildren >>> isElem
selectE = getChildren >>> isElem

transformE :: [Question] -> IOSLA (XIOState ()) XmlTree Exam
transformE qs = proc el -> do
  quiz <- getName -< el
  title <- getAttrValue "title" -< el
  passpercentage <- getAttrValue "passpercentage" -< el
  returnA -< (makeE quiz) (pack title) (readPassPercentage passpercentage) qs


transformQ :: [Answer] -> IOSLA (XIOState ()) XmlTree Question
transformQ as = proc el -> do
  question <- getName -< el
  content  <- getAttrValue "content" -< el
  returnA  -< (makeQ question) (pack content) as


transformA :: IOSLA (XIOState ()) XmlTree Answer
transformA = proc el -> do
  answer  <- getName -< el
  correct <- getAttrValue "correct" -< el
  comment <- withDefault (Just . pack ^<< getText <<< getChildren) Nothing -< el
  returnA -< (makeA answer) (fromMaybe "" comment) (elem correct ["true", "True"])



main :: IO ()
main = do
  as <- runX $ readDocument [withRelaxNG "transactions.rng"] "transactions.xml"
               >>> selectA >>> transformA
  qs <- mapM (\x -> runX $ readDocument [withRelaxNG "transactions.rng"] "transactions.xml"
               >>> selectQ >>> transformQ x) $ chunksOf 4 as
  ex <- runX $ readDocument [withRelaxNG "transactions.rng"] "transactions.xml"
              >>> selectE >>> transformE (fst $ foldl (\(x,y) q -> (x ++ [q!!y], y + 1)) ([],0) qs)

  putStrLn "hi"



